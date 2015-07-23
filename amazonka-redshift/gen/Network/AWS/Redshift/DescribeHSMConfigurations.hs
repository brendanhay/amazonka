{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeHSMConfigurations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Amazon Redshift HSM
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
    , dhsmcrqTagValues
    , dhsmcrqHSMConfigurationIdentifier
    , dhsmcrqTagKeys
    , dhsmcrqMaxRecords
    , dhsmcrqMarker

    -- * Response
    , DescribeHSMConfigurationsResponse
    -- ** Response constructor
    , describeHSMConfigurationsResponse
    -- ** Response lenses
    , dhcrsMarker
    , dhcrsHSMConfigurations
    , dhcrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeHSMConfigurations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhsmcrqTagValues'
--
-- * 'dhsmcrqHSMConfigurationIdentifier'
--
-- * 'dhsmcrqTagKeys'
--
-- * 'dhsmcrqMaxRecords'
--
-- * 'dhsmcrqMarker'
data DescribeHSMConfigurations = DescribeHSMConfigurations'
    { _dhsmcrqTagValues                  :: !(Maybe [Text])
    , _dhsmcrqHSMConfigurationIdentifier :: !(Maybe Text)
    , _dhsmcrqTagKeys                    :: !(Maybe [Text])
    , _dhsmcrqMaxRecords                 :: !(Maybe Int)
    , _dhsmcrqMarker                     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHSMConfigurations' smart constructor.
describeHSMConfigurations :: DescribeHSMConfigurations
describeHSMConfigurations =
    DescribeHSMConfigurations'
    { _dhsmcrqTagValues = Nothing
    , _dhsmcrqHSMConfigurationIdentifier = Nothing
    , _dhsmcrqTagKeys = Nothing
    , _dhsmcrqMaxRecords = Nothing
    , _dhsmcrqMarker = Nothing
    }

-- | A tag value or values for which you want to return all matching HSM
-- configurations that are associated with the specified tag value or
-- values. For example, suppose that you have HSM configurations that are
-- tagged with values called @admin@ and @test@. If you specify both of
-- these tag values in the request, Amazon Redshift returns a response with
-- the HSM configurations that have either or both of these tag values
-- associated with them.
dhsmcrqTagValues :: Lens' DescribeHSMConfigurations [Text]
dhsmcrqTagValues = lens _dhsmcrqTagValues (\ s a -> s{_dhsmcrqTagValues = a}) . _Default;

-- | The identifier of a specific Amazon Redshift HSM configuration to be
-- described. If no identifier is specified, information is returned for
-- all HSM configurations owned by your AWS customer account.
dhsmcrqHSMConfigurationIdentifier :: Lens' DescribeHSMConfigurations (Maybe Text)
dhsmcrqHSMConfigurationIdentifier = lens _dhsmcrqHSMConfigurationIdentifier (\ s a -> s{_dhsmcrqHSMConfigurationIdentifier = a});

-- | A tag key or keys for which you want to return all matching HSM
-- configurations that are associated with the specified key or keys. For
-- example, suppose that you have HSM configurations that are tagged with
-- keys called @owner@ and @environment@. If you specify both of these tag
-- keys in the request, Amazon Redshift returns a response with the HSM
-- configurations that have either or both of these tag keys associated
-- with them.
dhsmcrqTagKeys :: Lens' DescribeHSMConfigurations [Text]
dhsmcrqTagKeys = lens _dhsmcrqTagKeys (\ s a -> s{_dhsmcrqTagKeys = a}) . _Default;

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- Default: @100@
--
-- Constraints: minimum 20, maximum 100.
dhsmcrqMaxRecords :: Lens' DescribeHSMConfigurations (Maybe Int)
dhsmcrqMaxRecords = lens _dhsmcrqMaxRecords (\ s a -> s{_dhsmcrqMaxRecords = a});

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeHsmConfigurations
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
dhsmcrqMarker :: Lens' DescribeHSMConfigurations (Maybe Text)
dhsmcrqMarker = lens _dhsmcrqMarker (\ s a -> s{_dhsmcrqMarker = a});

instance AWSPager DescribeHSMConfigurations where
        page rq rs
          | stop (rs ^. dhcrsMarker) = Nothing
          | stop (rs ^. dhcrsHSMConfigurations) = Nothing
          | otherwise =
            Just $ rq & dhsmcrqMarker .~ rs ^. dhcrsMarker

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
                 toQuery
                   (toQueryList "TagValue" <$> _dhsmcrqTagValues),
               "HsmConfigurationIdentifier" =:
                 _dhsmcrqHSMConfigurationIdentifier,
               "TagKeys" =:
                 toQuery (toQueryList "TagKey" <$> _dhsmcrqTagKeys),
               "MaxRecords" =: _dhsmcrqMaxRecords,
               "Marker" =: _dhsmcrqMarker]

-- |
--
-- /See:/ 'describeHSMConfigurationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcrsMarker'
--
-- * 'dhcrsHSMConfigurations'
--
-- * 'dhcrsStatus'
data DescribeHSMConfigurationsResponse = DescribeHSMConfigurationsResponse'
    { _dhcrsMarker            :: !(Maybe Text)
    , _dhcrsHSMConfigurations :: !(Maybe [HSMConfiguration])
    , _dhcrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeHSMConfigurationsResponse' smart constructor.
describeHSMConfigurationsResponse :: Int -> DescribeHSMConfigurationsResponse
describeHSMConfigurationsResponse pStatus_ =
    DescribeHSMConfigurationsResponse'
    { _dhcrsMarker = Nothing
    , _dhcrsHSMConfigurations = Nothing
    , _dhcrsStatus = pStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
dhcrsMarker :: Lens' DescribeHSMConfigurationsResponse (Maybe Text)
dhcrsMarker = lens _dhcrsMarker (\ s a -> s{_dhcrsMarker = a});

-- | A list of Amazon Redshift HSM configurations.
dhcrsHSMConfigurations :: Lens' DescribeHSMConfigurationsResponse [HSMConfiguration]
dhcrsHSMConfigurations = lens _dhcrsHSMConfigurations (\ s a -> s{_dhcrsHSMConfigurations = a}) . _Default;

-- | FIXME: Undocumented member.
dhcrsStatus :: Lens' DescribeHSMConfigurationsResponse Int
dhcrsStatus = lens _dhcrsStatus (\ s a -> s{_dhcrsStatus = a});
