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
-- Module      : Network.AWS.Discovery.DescribeExportConfigurations
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a given export process. You can retrieve status from a maximum of 100 processes.
--
--
module Network.AWS.Discovery.DescribeExportConfigurations
    (
    -- * Creating a Request
      describeExportConfigurations
    , DescribeExportConfigurations
    -- * Request Lenses
    , decNextToken
    , decExportIds
    , decMaxResults

    -- * Destructuring the Response
    , describeExportConfigurationsResponse
    , DescribeExportConfigurationsResponse
    -- * Response Lenses
    , decrsNextToken
    , decrsExportsInfo
    , decrsResponseStatus
    ) where

import           Network.AWS.Discovery.Types
import           Network.AWS.Discovery.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeExportConfigurations' smart constructor.
data DescribeExportConfigurations = DescribeExportConfigurations'
    { _decNextToken  :: !(Maybe Text)
    , _decExportIds  :: !(Maybe [Text])
    , _decMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeExportConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decNextToken' - A token to get the next set of results. For example, if you specified 100 IDs for @DescribeExportConfigurationsRequest$exportIds@ but set @DescribeExportConfigurationsRequest$maxResults@ to 10, you will get results in a set of 10. Use the token in the query to get the next set of 10.
--
-- * 'decExportIds' - A unique identifier that you can use to query the export status.
--
-- * 'decMaxResults' - The maximum number of results that you want to display as a part of the query.
describeExportConfigurations
    :: DescribeExportConfigurations
describeExportConfigurations =
    DescribeExportConfigurations'
    { _decNextToken = Nothing
    , _decExportIds = Nothing
    , _decMaxResults = Nothing
    }

-- | A token to get the next set of results. For example, if you specified 100 IDs for @DescribeExportConfigurationsRequest$exportIds@ but set @DescribeExportConfigurationsRequest$maxResults@ to 10, you will get results in a set of 10. Use the token in the query to get the next set of 10.
decNextToken :: Lens' DescribeExportConfigurations (Maybe Text)
decNextToken = lens _decNextToken (\ s a -> s{_decNextToken = a});

-- | A unique identifier that you can use to query the export status.
decExportIds :: Lens' DescribeExportConfigurations [Text]
decExportIds = lens _decExportIds (\ s a -> s{_decExportIds = a}) . _Default . _Coerce;

-- | The maximum number of results that you want to display as a part of the query.
decMaxResults :: Lens' DescribeExportConfigurations (Maybe Int)
decMaxResults = lens _decMaxResults (\ s a -> s{_decMaxResults = a});

instance AWSRequest DescribeExportConfigurations
         where
        type Rs DescribeExportConfigurations =
             DescribeExportConfigurationsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 DescribeExportConfigurationsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "exportsInfo" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeExportConfigurations

instance NFData DescribeExportConfigurations

instance ToHeaders DescribeExportConfigurations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DescribeExportConfigurations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeExportConfigurations where
        toJSON DescribeExportConfigurations'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _decNextToken,
                  ("exportIds" .=) <$> _decExportIds,
                  ("maxResults" .=) <$> _decMaxResults])

instance ToPath DescribeExportConfigurations where
        toPath = const "/"

instance ToQuery DescribeExportConfigurations where
        toQuery = const mempty

-- | /See:/ 'describeExportConfigurationsResponse' smart constructor.
data DescribeExportConfigurationsResponse = DescribeExportConfigurationsResponse'
    { _decrsNextToken      :: !(Maybe Text)
    , _decrsExportsInfo    :: !(Maybe [ExportInfo])
    , _decrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeExportConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decrsNextToken' - A token to get the next set of results. For example, if you specified 100 IDs for @DescribeExportConfigurationsRequest$exportIds@ but set @DescribeExportConfigurationsRequest$maxResults@ to 10, you will get results in a set of 10. Use the token in the query to get the next set of 10.
--
-- * 'decrsExportsInfo' - Returns export details. When the status is complete, the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
--
-- * 'decrsResponseStatus' - -- | The response status code.
describeExportConfigurationsResponse
    :: Int -- ^ 'decrsResponseStatus'
    -> DescribeExportConfigurationsResponse
describeExportConfigurationsResponse pResponseStatus_ =
    DescribeExportConfigurationsResponse'
    { _decrsNextToken = Nothing
    , _decrsExportsInfo = Nothing
    , _decrsResponseStatus = pResponseStatus_
    }

-- | A token to get the next set of results. For example, if you specified 100 IDs for @DescribeExportConfigurationsRequest$exportIds@ but set @DescribeExportConfigurationsRequest$maxResults@ to 10, you will get results in a set of 10. Use the token in the query to get the next set of 10.
decrsNextToken :: Lens' DescribeExportConfigurationsResponse (Maybe Text)
decrsNextToken = lens _decrsNextToken (\ s a -> s{_decrsNextToken = a});

-- | Returns export details. When the status is complete, the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
decrsExportsInfo :: Lens' DescribeExportConfigurationsResponse [ExportInfo]
decrsExportsInfo = lens _decrsExportsInfo (\ s a -> s{_decrsExportsInfo = a}) . _Default . _Coerce;

-- | -- | The response status code.
decrsResponseStatus :: Lens' DescribeExportConfigurationsResponse Int
decrsResponseStatus = lens _decrsResponseStatus (\ s a -> s{_decrsResponseStatus = a});

instance NFData DescribeExportConfigurationsResponse
