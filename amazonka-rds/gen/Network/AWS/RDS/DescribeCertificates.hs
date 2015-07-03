{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.DescribeCertificates
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the set of CA certificates provided by Amazon RDS for this AWS
-- account.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DescribeCertificates.html>
module Network.AWS.RDS.DescribeCertificates
    (
    -- * Request
      DescribeCertificates
    -- ** Request constructor
    , describeCertificates
    -- ** Request lenses
    , dcFilters
    , dcCertificateIdentifier
    , dcMaxRecords
    , dcMarker

    -- * Response
    , DescribeCertificatesResponse
    -- ** Response constructor
    , describeCertificatesResponse
    -- ** Response lenses
    , dcrCertificates
    , dcrMarker
    , dcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeCertificates' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcFilters'
--
-- * 'dcCertificateIdentifier'
--
-- * 'dcMaxRecords'
--
-- * 'dcMarker'
data DescribeCertificates = DescribeCertificates'
    { _dcFilters               :: !(Maybe [Filter])
    , _dcCertificateIdentifier :: !(Maybe Text)
    , _dcMaxRecords            :: !(Maybe Int)
    , _dcMarker                :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeCertificates' smart constructor.
describeCertificates :: DescribeCertificates
describeCertificates =
    DescribeCertificates'
    { _dcFilters = Nothing
    , _dcCertificateIdentifier = Nothing
    , _dcMaxRecords = Nothing
    , _dcMarker = Nothing
    }

-- | This parameter is not currently supported.
dcFilters :: Lens' DescribeCertificates [Filter]
dcFilters = lens _dcFilters (\ s a -> s{_dcFilters = a}) . _Default;

-- | The user-supplied certificate identifier. If this parameter is
-- specified, information for only the identified certificate is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
dcCertificateIdentifier :: Lens' DescribeCertificates (Maybe Text)
dcCertificateIdentifier = lens _dcCertificateIdentifier (\ s a -> s{_dcCertificateIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100
dcMaxRecords :: Lens' DescribeCertificates (Maybe Int)
dcMaxRecords = lens _dcMaxRecords (\ s a -> s{_dcMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeCertificates
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
dcMarker :: Lens' DescribeCertificates (Maybe Text)
dcMarker = lens _dcMarker (\ s a -> s{_dcMarker = a});

instance AWSRequest DescribeCertificates where
        type Sv DescribeCertificates = RDS
        type Rs DescribeCertificates =
             DescribeCertificatesResponse
        request = post
        response
          = receiveXMLWrapper "DescribeCertificatesResult"
              (\ s h x ->
                 DescribeCertificatesResponse' <$>
                   (x .@? "Certificates" .!@ mempty >>=
                      may (parseXMLList "Certificate"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCertificates where
        toHeaders = const mempty

instance ToPath DescribeCertificates where
        toPath = const "/"

instance ToQuery DescribeCertificates where
        toQuery DescribeCertificates'{..}
          = mconcat
              ["Action" =: ("DescribeCertificates" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dcFilters),
               "CertificateIdentifier" =: _dcCertificateIdentifier,
               "MaxRecords" =: _dcMaxRecords, "Marker" =: _dcMarker]

-- | Data returned by the __DescribeCertificates__ action.
--
-- /See:/ 'describeCertificatesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrCertificates'
--
-- * 'dcrMarker'
--
-- * 'dcrStatus'
data DescribeCertificatesResponse = DescribeCertificatesResponse'
    { _dcrCertificates :: !(Maybe [Certificate])
    , _dcrMarker       :: !(Maybe Text)
    , _dcrStatus       :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeCertificatesResponse' smart constructor.
describeCertificatesResponse :: Int -> DescribeCertificatesResponse
describeCertificatesResponse pStatus =
    DescribeCertificatesResponse'
    { _dcrCertificates = Nothing
    , _dcrMarker = Nothing
    , _dcrStatus = pStatus
    }

-- | The list of Certificate objects for the AWS account.
dcrCertificates :: Lens' DescribeCertificatesResponse [Certificate]
dcrCertificates = lens _dcrCertificates (\ s a -> s{_dcrCertificates = a}) . _Default;

-- | An optional pagination token provided by a previous DescribeCertificates
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@ .
dcrMarker :: Lens' DescribeCertificatesResponse (Maybe Text)
dcrMarker = lens _dcrMarker (\ s a -> s{_dcrMarker = a});

-- | FIXME: Undocumented member.
dcrStatus :: Lens' DescribeCertificatesResponse Int
dcrStatus = lens _dcrStatus (\ s a -> s{_dcrStatus = a});
