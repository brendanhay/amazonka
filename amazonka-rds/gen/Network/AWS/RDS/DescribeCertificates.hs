{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeCertificates
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the set of CA certificates provided by Amazon RDS for this AWS
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
    , dcrqFilters
    , dcrqCertificateIdentifier
    , dcrqMaxRecords
    , dcrqMarker

    -- * Response
    , DescribeCertificatesResponse
    -- ** Response constructor
    , describeCertificatesResponse
    -- ** Response lenses
    , dcrsCertificates
    , dcrsMarker
    , dcrsStatus
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
-- * 'dcrqFilters'
--
-- * 'dcrqCertificateIdentifier'
--
-- * 'dcrqMaxRecords'
--
-- * 'dcrqMarker'
data DescribeCertificates = DescribeCertificates'
    { _dcrqFilters               :: !(Maybe [Filter])
    , _dcrqCertificateIdentifier :: !(Maybe Text)
    , _dcrqMaxRecords            :: !(Maybe Int)
    , _dcrqMarker                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCertificates' smart constructor.
describeCertificates :: DescribeCertificates
describeCertificates =
    DescribeCertificates'
    { _dcrqFilters = Nothing
    , _dcrqCertificateIdentifier = Nothing
    , _dcrqMaxRecords = Nothing
    , _dcrqMarker = Nothing
    }

-- | This parameter is not currently supported.
dcrqFilters :: Lens' DescribeCertificates [Filter]
dcrqFilters = lens _dcrqFilters (\ s a -> s{_dcrqFilters = a}) . _Default;

-- | The user-supplied certificate identifier. If this parameter is
-- specified, information for only the identified certificate is returned.
-- This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
dcrqCertificateIdentifier :: Lens' DescribeCertificates (Maybe Text)
dcrqCertificateIdentifier = lens _dcrqCertificateIdentifier (\ s a -> s{_dcrqCertificateIdentifier = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100
dcrqMaxRecords :: Lens' DescribeCertificates (Maybe Int)
dcrqMaxRecords = lens _dcrqMaxRecords (\ s a -> s{_dcrqMaxRecords = a});

-- | An optional pagination token provided by a previous DescribeCertificates
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
dcrqMarker :: Lens' DescribeCertificates (Maybe Text)
dcrqMarker = lens _dcrqMarker (\ s a -> s{_dcrqMarker = a});

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
                 toQuery (toQueryList "Filter" <$> _dcrqFilters),
               "CertificateIdentifier" =:
                 _dcrqCertificateIdentifier,
               "MaxRecords" =: _dcrqMaxRecords,
               "Marker" =: _dcrqMarker]

-- | Data returned by the __DescribeCertificates__ action.
--
-- /See:/ 'describeCertificatesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsCertificates'
--
-- * 'dcrsMarker'
--
-- * 'dcrsStatus'
data DescribeCertificatesResponse = DescribeCertificatesResponse'
    { _dcrsCertificates :: !(Maybe [Certificate])
    , _dcrsMarker       :: !(Maybe Text)
    , _dcrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCertificatesResponse' smart constructor.
describeCertificatesResponse :: Int -> DescribeCertificatesResponse
describeCertificatesResponse pStatus_ =
    DescribeCertificatesResponse'
    { _dcrsCertificates = Nothing
    , _dcrsMarker = Nothing
    , _dcrsStatus = pStatus_
    }

-- | The list of Certificate objects for the AWS account.
dcrsCertificates :: Lens' DescribeCertificatesResponse [Certificate]
dcrsCertificates = lens _dcrsCertificates (\ s a -> s{_dcrsCertificates = a}) . _Default;

-- | An optional pagination token provided by a previous DescribeCertificates
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@ .
dcrsMarker :: Lens' DescribeCertificatesResponse (Maybe Text)
dcrsMarker = lens _dcrsMarker (\ s a -> s{_dcrsMarker = a});

-- | FIXME: Undocumented member.
dcrsStatus :: Lens' DescribeCertificatesResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});
