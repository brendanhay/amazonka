{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DescribeHSMConfigurations
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

-- | Returns information about the specified Amazon Redshift HSM
-- configuration. If no configuration ID is specified, returns information
-- about all the HSM configurations owned by your AWS customer account.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all HSM connections that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- HSM connections that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, HSM
-- connections are returned regardless of whether they have tag keys or
-- values associated with them.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeHSMConfigurations.html>
module Network.AWS.Redshift.DescribeHSMConfigurations
    (
    -- * Request
      DescribeHSMConfigurations
    -- ** Request constructor
    , describeHSMConfigurations
    -- ** Request lenses
    , dhsmcTagValues
    , dhsmcHSMConfigurationIdentifier
    , dhsmcTagKeys
    , dhsmcMaxRecords
    , dhsmcMarker

    -- * Response
    , DescribeHSMConfigurationsResponse
    -- ** Response constructor
    , describeHSMConfigurationsResponse
    -- ** Response lenses
    , dhcrMarker
    , dhcrHSMConfigurations
    , dhcrStatusCode
    ) where

import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
-- /See:/ 'describeHSMConfigurations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhsmcTagValues'
--
-- * 'dhsmcHSMConfigurationIdentifier'
--
-- * 'dhsmcTagKeys'
--
-- * 'dhsmcMaxRecords'
--
-- * 'dhsmcMarker'
data DescribeHSMConfigurations = DescribeHSMConfigurations'{_dhsmcTagValues :: Maybe [Text], _dhsmcHSMConfigurationIdentifier :: Maybe Text, _dhsmcTagKeys :: Maybe [Text], _dhsmcMaxRecords :: Maybe Int, _dhsmcMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeHSMConfigurations' smart constructor.
describeHSMConfigurations :: DescribeHSMConfigurations
describeHSMConfigurations = DescribeHSMConfigurations'{_dhsmcTagValues = Nothing, _dhsmcHSMConfigurationIdentifier = Nothing, _dhsmcTagKeys = Nothing, _dhsmcMaxRecords = Nothing, _dhsmcMarker = Nothing};

-- | A tag value or values for which you want to return all matching HSM
-- configurations that are associated with the specified tag value or
-- values. For example, suppose that you have HSM configurations that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM configurations that have either or both of these tag values
-- associated with them.
dhsmcTagValues :: Lens' DescribeHSMConfigurations [Text]
dhsmcTagValues = lens _dhsmcTagValues (\ s a -> s{_dhsmcTagValues = a}) . _Default;

-- | The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for
-- all HSM configurations owned by your AWS customer account.
dhsmcHSMConfigurationIdentifier :: Lens' DescribeHSMConfigurations (Maybe Text)
dhsmcHSMConfigurationIdentifier = lens _dhsmcHSMConfigurationIdentifier (\ s a -> s{_dhsmcHSMConfigurationIdentifier = a});

-- | A tag key or keys for which you want to return all matching HSM
-- configurations that are associated with the specified key or keys. For
-- example, suppose that you have HSM configurations that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the HSM
-- configurations that have either or both of these tag keys associated
-- with them.
dhsmcTagKeys :: Lens' DescribeHSMConfigurations [Text]
dhsmcTagKeys = lens _dhsmcTagKeys (\ s a -> s{_dhsmcTagKeys = a}) . _Default;

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dhsmcMaxRecords :: Lens' DescribeHSMConfigurations (Maybe Int)
dhsmcMaxRecords = lens _dhsmcMaxRecords (\ s a -> s{_dhsmcMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmConfigurations
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dhsmcMarker :: Lens' DescribeHSMConfigurations (Maybe Text)
dhsmcMarker = lens _dhsmcMarker (\ s a -> s{_dhsmcMarker = a});

instance AWSPager DescribeHSMConfigurations where
        page rq rs
          | stop (rs ^. dhcrMarker) = Nothing
          | stop (rs ^. dhcrHSMConfigurations) = Nothing
          | otherwise =
            Just $ rq & dhsmcMarker .~ rs ^. dhcrMarker

instance AWSRequest DescribeHSMConfigurations where
        type Sv DescribeHSMConfigurations = Redshift
        type Rs DescribeHSMConfigurations =
             DescribeHSMConfigurationsResponse
        request = post
        response
          = receiveXMLWrapper "DescribeHsmConfigurationsResult"
              (\ s h x ->
                 DescribeHSMConfigurationsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "HsmConfigurations" .!@ mempty >>=
                        may (parseXMLList "HsmConfiguration"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeHSMConfigurations where
        toHeaders = const mempty

instance ToPath DescribeHSMConfigurations where
        toPath = const "/"

instance ToQuery DescribeHSMConfigurations where
        toQuery DescribeHSMConfigurations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeHSMConfigurations" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dhsmcTagValues),
               "HsmConfigurationIdentifier" =:
                 _dhsmcHSMConfigurationIdentifier,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dhsmcTagKeys),
               "MaxRecords" =: _dhsmcMaxRecords,
               "Marker" =: _dhsmcMarker]

-- |
--
-- /See:/ 'describeHSMConfigurationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcrMarker'
--
-- * 'dhcrHSMConfigurations'
--
-- * 'dhcrStatusCode'
data DescribeHSMConfigurationsResponse = DescribeHSMConfigurationsResponse'{_dhcrMarker :: Maybe Text, _dhcrHSMConfigurations :: Maybe [HSMConfiguration], _dhcrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeHSMConfigurationsResponse' smart constructor.
describeHSMConfigurationsResponse :: Int -> DescribeHSMConfigurationsResponse
describeHSMConfigurationsResponse pStatusCode = DescribeHSMConfigurationsResponse'{_dhcrMarker = Nothing, _dhcrHSMConfigurations = Nothing, _dhcrStatusCode = pStatusCode};

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dhcrMarker :: Lens' DescribeHSMConfigurationsResponse (Maybe Text)
dhcrMarker = lens _dhcrMarker (\ s a -> s{_dhcrMarker = a});

-- | A list of Amazon Redshift HSM configurations.
dhcrHSMConfigurations :: Lens' DescribeHSMConfigurationsResponse [HSMConfiguration]
dhcrHSMConfigurations = lens _dhcrHSMConfigurations (\ s a -> s{_dhcrHSMConfigurations = a}) . _Default;

-- | FIXME: Undocumented member.
dhcrStatusCode :: Lens' DescribeHSMConfigurationsResponse Int
dhcrStatusCode = lens _dhcrStatusCode (\ s a -> s{_dhcrStatusCode = a});
