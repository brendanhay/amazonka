{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeHSMConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Amazon Redshift HSM configuration. If no configuration ID is specified, returns information about all the HSM configurations owned by your AWS customer account.
--
--
-- If you specify both tag keys and tag values in the same request, Amazon Redshift returns all HSM connections that match any combination of the specified keys and values. For example, if you have @owner@ and @environment@ for tag keys, and @admin@ and @test@ for tag values, all HSM connections that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, HSM connections are returned regardless of whether they have tag keys or values associated with them.
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeHSMConfigurations
    (
    -- * Creating a Request
      describeHSMConfigurations
    , DescribeHSMConfigurations
    -- * Request Lenses
    , dhsmcTagValues
    , dhsmcHSMConfigurationIdentifier
    , dhsmcTagKeys
    , dhsmcMarker
    , dhsmcMaxRecords

    -- * Destructuring the Response
    , describeHSMConfigurationsResponse
    , DescribeHSMConfigurationsResponse
    -- * Response Lenses
    , dhcrsMarker
    , dhcrsHSMConfigurations
    , dhcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeHSMConfigurations' smart constructor.
data DescribeHSMConfigurations = DescribeHSMConfigurations'
  { _dhsmcTagValues                  :: !(Maybe [Text])
  , _dhsmcHSMConfigurationIdentifier :: !(Maybe Text)
  , _dhsmcTagKeys                    :: !(Maybe [Text])
  , _dhsmcMarker                     :: !(Maybe Text)
  , _dhsmcMaxRecords                 :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHSMConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhsmcTagValues' - A tag value or values for which you want to return all matching HSM configurations that are associated with the specified tag value or values. For example, suppose that you have HSM configurations that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag values associated with them.
--
-- * 'dhsmcHSMConfigurationIdentifier' - The identifier of a specific Amazon Redshift HSM configuration to be described. If no identifier is specified, information is returned for all HSM configurations owned by your AWS customer account.
--
-- * 'dhsmcTagKeys' - A tag key or keys for which you want to return all matching HSM configurations that are associated with the specified key or keys. For example, suppose that you have HSM configurations that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag keys associated with them.
--
-- * 'dhsmcMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmConfigurations' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dhsmcMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
describeHSMConfigurations
    :: DescribeHSMConfigurations
describeHSMConfigurations =
  DescribeHSMConfigurations'
    { _dhsmcTagValues = Nothing
    , _dhsmcHSMConfigurationIdentifier = Nothing
    , _dhsmcTagKeys = Nothing
    , _dhsmcMarker = Nothing
    , _dhsmcMaxRecords = Nothing
    }


-- | A tag value or values for which you want to return all matching HSM configurations that are associated with the specified tag value or values. For example, suppose that you have HSM configurations that are tagged with values called @admin@ and @test@ . If you specify both of these tag values in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag values associated with them.
dhsmcTagValues :: Lens' DescribeHSMConfigurations [Text]
dhsmcTagValues = lens _dhsmcTagValues (\ s a -> s{_dhsmcTagValues = a}) . _Default . _Coerce

-- | The identifier of a specific Amazon Redshift HSM configuration to be described. If no identifier is specified, information is returned for all HSM configurations owned by your AWS customer account.
dhsmcHSMConfigurationIdentifier :: Lens' DescribeHSMConfigurations (Maybe Text)
dhsmcHSMConfigurationIdentifier = lens _dhsmcHSMConfigurationIdentifier (\ s a -> s{_dhsmcHSMConfigurationIdentifier = a})

-- | A tag key or keys for which you want to return all matching HSM configurations that are associated with the specified key or keys. For example, suppose that you have HSM configurations that are tagged with keys called @owner@ and @environment@ . If you specify both of these tag keys in the request, Amazon Redshift returns a response with the HSM configurations that have either or both of these tag keys associated with them.
dhsmcTagKeys :: Lens' DescribeHSMConfigurations [Text]
dhsmcTagKeys = lens _dhsmcTagKeys (\ s a -> s{_dhsmcTagKeys = a}) . _Default . _Coerce

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeHsmConfigurations' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dhsmcMarker :: Lens' DescribeHSMConfigurations (Maybe Text)
dhsmcMarker = lens _dhsmcMarker (\ s a -> s{_dhsmcMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dhsmcMaxRecords :: Lens' DescribeHSMConfigurations (Maybe Int)
dhsmcMaxRecords = lens _dhsmcMaxRecords (\ s a -> s{_dhsmcMaxRecords = a})

instance AWSPager DescribeHSMConfigurations where
        page rq rs
          | stop (rs ^. dhcrsMarker) = Nothing
          | stop (rs ^. dhcrsHSMConfigurations) = Nothing
          | otherwise =
            Just $ rq & dhsmcMarker .~ rs ^. dhcrsMarker

instance AWSRequest DescribeHSMConfigurations where
        type Rs DescribeHSMConfigurations =
             DescribeHSMConfigurationsResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DescribeHsmConfigurationsResult"
              (\ s h x ->
                 DescribeHSMConfigurationsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "HsmConfigurations" .!@ mempty >>=
                        may (parseXMLList "HsmConfiguration"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeHSMConfigurations where

instance NFData DescribeHSMConfigurations where

instance ToHeaders DescribeHSMConfigurations where
        toHeaders = const mempty

instance ToPath DescribeHSMConfigurations where
        toPath = const "/"

instance ToQuery DescribeHSMConfigurations where
        toQuery DescribeHSMConfigurations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeHsmConfigurations" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "TagValues" =:
                 toQuery (toQueryList "TagValue" <$> _dhsmcTagValues),
               "HsmConfigurationIdentifier" =:
                 _dhsmcHSMConfigurationIdentifier,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dhsmcTagKeys),
               "Marker" =: _dhsmcMarker,
               "MaxRecords" =: _dhsmcMaxRecords]

-- |
--
--
--
-- /See:/ 'describeHSMConfigurationsResponse' smart constructor.
data DescribeHSMConfigurationsResponse = DescribeHSMConfigurationsResponse'
  { _dhcrsMarker            :: !(Maybe Text)
  , _dhcrsHSMConfigurations :: !(Maybe [HSMConfiguration])
  , _dhcrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeHSMConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcrsMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dhcrsHSMConfigurations' - A list of @HsmConfiguration@ objects.
--
-- * 'dhcrsResponseStatus' - -- | The response status code.
describeHSMConfigurationsResponse
    :: Int -- ^ 'dhcrsResponseStatus'
    -> DescribeHSMConfigurationsResponse
describeHSMConfigurationsResponse pResponseStatus_ =
  DescribeHSMConfigurationsResponse'
    { _dhcrsMarker = Nothing
    , _dhcrsHSMConfigurations = Nothing
    , _dhcrsResponseStatus = pResponseStatus_
    }


-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dhcrsMarker :: Lens' DescribeHSMConfigurationsResponse (Maybe Text)
dhcrsMarker = lens _dhcrsMarker (\ s a -> s{_dhcrsMarker = a})

-- | A list of @HsmConfiguration@ objects.
dhcrsHSMConfigurations :: Lens' DescribeHSMConfigurationsResponse [HSMConfiguration]
dhcrsHSMConfigurations = lens _dhcrsHSMConfigurations (\ s a -> s{_dhcrsHSMConfigurations = a}) . _Default . _Coerce

-- | -- | The response status code.
dhcrsResponseStatus :: Lens' DescribeHSMConfigurationsResponse Int
dhcrsResponseStatus = lens _dhcrsResponseStatus (\ s a -> s{_dhcrsResponseStatus = a})

instance NFData DescribeHSMConfigurationsResponse
         where
