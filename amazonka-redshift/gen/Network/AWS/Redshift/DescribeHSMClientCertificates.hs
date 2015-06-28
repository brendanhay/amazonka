{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.DescribeHSMClientCertificates
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about the specified HSM client certificate. If no
-- certificate ID is specified, returns information about all the HSM
-- certificates owned by your AWS customer account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all HSM client certificates that match any combination
-- of the specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- HSM client certificates that have any combination of those values are
-- returned.
--
-- If both tag keys and values are omitted from the request, HSM client
-- certificates are returned regardless of whether they have tag keys or
-- values associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeHSMClientCertificates.html>
module Network.AWS.Redshift.DescribeHSMClientCertificates
    (
    -- * Request
      DescribeHSMClientCertificates
    -- ** Request constructor
    , describeHSMClientCertificates
    -- ** Request lenses
    , dhccTagValues
    , dhccTagKeys
    , dhccHSMClientCertificateIdentifier
    , dhccMaxRecords
    , dhccMarker

    -- * Response
    , DescribeHSMClientCertificatesResponse
    -- ** Response constructor
    , describeHSMClientCertificatesResponse
    -- ** Response lenses
    , dhccrMarker
    , dhccrHSMClientCertificates
    , dhccrStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeHSMClientCertificates' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhccTagValues'
--
-- * 'dhccTagKeys'
--
-- * 'dhccHSMClientCertificateIdentifier'
--
-- * 'dhccMaxRecords'
--
-- * 'dhccMarker'
data DescribeHSMClientCertificates = DescribeHSMClientCertificates'
    { _dhccTagValues                      :: !(Maybe [Text])
    , _dhccTagKeys                        :: !(Maybe [Text])
    , _dhccHSMClientCertificateIdentifier :: !(Maybe Text)
    , _dhccMaxRecords                     :: !(Maybe Int)
    , _dhccMarker                         :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DescribeHSMClientCertificates' smart constructor.
describeHSMClientCertificates :: DescribeHSMClientCertificates
describeHSMClientCertificates =
    DescribeHSMClientCertificates'
    { _dhccTagValues = Nothing
    , _dhccTagKeys = Nothing
    , _dhccHSMClientCertificateIdentifier = Nothing
    , _dhccMaxRecords = Nothing
    , _dhccMarker = Nothing
    }

-- | A tag value or values for which you want to return all matching HSM
-- client certificates that are associated with the specified tag value or
-- values. For example, suppose that you have HSM client certificates that
-- are tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM client certificates that have either or both of these tag values
-- associated with them.
dhccTagValues :: Lens' DescribeHSMClientCertificates [Text]
dhccTagValues = lens _dhccTagValues (\ s a -> s{_dhccTagValues = a}) . _Default;

-- | A tag key or keys for which you want to return all matching HSM client
-- certificates that are associated with the specified key or keys. For
-- example, suppose that you have HSM client certificates that are tagged
-- with keys called @owner@ and @environment@. If you specify both of these
-- tag keys in the request, Amazon Redshift returns a response with the HSM
-- client certificates that have either or both of these tag keys
-- associated with them.
dhccTagKeys :: Lens' DescribeHSMClientCertificates [Text]
dhccTagKeys = lens _dhccTagKeys (\ s a -> s{_dhccTagKeys = a}) . _Default;

-- | The identifier of a specific HSM client certificate for which you want
-- information. If no identifier is specified, information is returned for
-- all HSM client certificates owned by your AWS customer account.
dhccHSMClientCertificateIdentifier :: Lens' DescribeHSMClientCertificates (Maybe Text)
dhccHSMClientCertificateIdentifier = lens _dhccHSMClientCertificateIdentifier (\ s a -> s{_dhccHSMClientCertificateIdentifier = a});

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dhccMaxRecords :: Lens' DescribeHSMClientCertificates (Maybe Int)
dhccMaxRecords = lens _dhccMaxRecords (\ s a -> s{_dhccMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmClientCertificates
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dhccMarker :: Lens' DescribeHSMClientCertificates (Maybe Text)
dhccMarker = lens _dhccMarker (\ s a -> s{_dhccMarker = a});

instance AWSPager DescribeHSMClientCertificates where
        page rq rs
          | stop (rs ^. dhccrMarker) = Nothing
          | stop (rs ^. dhccrHSMClientCertificates) = Nothing
          | otherwise =
            Just $ rq & dhccMarker .~ rs ^. dhccrMarker

instance AWSRequest DescribeHSMClientCertificates
         where
        type Sv DescribeHSMClientCertificates = Redshift
        type Rs DescribeHSMClientCertificates =
             DescribeHSMClientCertificatesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeHsmClientCertificatesResult"
              (\ s h x ->
                 DescribeHSMClientCertificatesResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "HsmClientCertificates" .!@ mempty >>=
                        may (parseXMLList "HsmClientCertificate"))
                     <*> (pure s))

instance ToHeaders DescribeHSMClientCertificates
         where
        toHeaders = const mempty

instance ToPath DescribeHSMClientCertificates where
        toPath = const "/"

instance ToQuery DescribeHSMClientCertificates where
        toQuery DescribeHSMClientCertificates'{..}
          = mconcat
              ["Action" =:
                 ("DescribeHSMClientCertificates" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dhccTagValues),
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dhccTagKeys),
               "HsmClientCertificateIdentifier" =:
                 _dhccHSMClientCertificateIdentifier,
               "MaxRecords" =: _dhccMaxRecords,
               "Marker" =: _dhccMarker]

-- |
--
-- /See:/ 'describeHSMClientCertificatesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhccrMarker'
--
-- * 'dhccrHSMClientCertificates'
--
-- * 'dhccrStatus'
data DescribeHSMClientCertificatesResponse = DescribeHSMClientCertificatesResponse'
    { _dhccrMarker                :: !(Maybe Text)
    , _dhccrHSMClientCertificates :: !(Maybe [HSMClientCertificate])
    , _dhccrStatus                :: !Status
    } deriving (Eq,Show)

-- | 'DescribeHSMClientCertificatesResponse' smart constructor.
describeHSMClientCertificatesResponse :: Status -> DescribeHSMClientCertificatesResponse
describeHSMClientCertificatesResponse pStatus =
    DescribeHSMClientCertificatesResponse'
    { _dhccrMarker = Nothing
    , _dhccrHSMClientCertificates = Nothing
    , _dhccrStatus = pStatus
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dhccrMarker :: Lens' DescribeHSMClientCertificatesResponse (Maybe Text)
dhccrMarker = lens _dhccrMarker (\ s a -> s{_dhccrMarker = a});

-- | A list of the identifiers for one or more HSM client certificates used
-- by Amazon Redshift clusters to store and retrieve database encryption
-- keys in an HSM.
dhccrHSMClientCertificates :: Lens' DescribeHSMClientCertificatesResponse [HSMClientCertificate]
dhccrHSMClientCertificates = lens _dhccrHSMClientCertificates (\ s a -> s{_dhccrHSMClientCertificates = a}) . _Default;

-- | FIXME: Undocumented member.
dhccrStatus :: Lens' DescribeHSMClientCertificatesResponse Status
dhccrStatus = lens _dhccrStatus (\ s a -> s{_dhccrStatus = a});
