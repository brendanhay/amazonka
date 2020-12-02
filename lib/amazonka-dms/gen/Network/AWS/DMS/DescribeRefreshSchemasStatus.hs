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
-- Module      : Network.AWS.DMS.DescribeRefreshSchemasStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of the RefreshSchemas operation.
--
--
module Network.AWS.DMS.DescribeRefreshSchemasStatus
    (
    -- * Creating a Request
      describeRefreshSchemasStatus
    , DescribeRefreshSchemasStatus
    -- * Request Lenses
    , drssEndpointARN

    -- * Destructuring the Response
    , describeRefreshSchemasStatusResponse
    , DescribeRefreshSchemasStatusResponse
    -- * Response Lenses
    , drssrsRefreshSchemasStatus
    , drssrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeRefreshSchemasStatus' smart constructor.
newtype DescribeRefreshSchemasStatus = DescribeRefreshSchemasStatus'
  { _drssEndpointARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRefreshSchemasStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drssEndpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
describeRefreshSchemasStatus
    :: Text -- ^ 'drssEndpointARN'
    -> DescribeRefreshSchemasStatus
describeRefreshSchemasStatus pEndpointARN_ =
  DescribeRefreshSchemasStatus' {_drssEndpointARN = pEndpointARN_}


-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
drssEndpointARN :: Lens' DescribeRefreshSchemasStatus Text
drssEndpointARN = lens _drssEndpointARN (\ s a -> s{_drssEndpointARN = a})

instance AWSRequest DescribeRefreshSchemasStatus
         where
        type Rs DescribeRefreshSchemasStatus =
             DescribeRefreshSchemasStatusResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRefreshSchemasStatusResponse' <$>
                   (x .?> "RefreshSchemasStatus") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeRefreshSchemasStatus where

instance NFData DescribeRefreshSchemasStatus where

instance ToHeaders DescribeRefreshSchemasStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DescribeRefreshSchemasStatus" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRefreshSchemasStatus where
        toJSON DescribeRefreshSchemasStatus'{..}
          = object
              (catMaybes
                 [Just ("EndpointArn" .= _drssEndpointARN)])

instance ToPath DescribeRefreshSchemasStatus where
        toPath = const "/"

instance ToQuery DescribeRefreshSchemasStatus where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeRefreshSchemasStatusResponse' smart constructor.
data DescribeRefreshSchemasStatusResponse = DescribeRefreshSchemasStatusResponse'
  { _drssrsRefreshSchemasStatus :: !(Maybe RefreshSchemasStatus)
  , _drssrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRefreshSchemasStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drssrsRefreshSchemasStatus' - The status of the schema.
--
-- * 'drssrsResponseStatus' - -- | The response status code.
describeRefreshSchemasStatusResponse
    :: Int -- ^ 'drssrsResponseStatus'
    -> DescribeRefreshSchemasStatusResponse
describeRefreshSchemasStatusResponse pResponseStatus_ =
  DescribeRefreshSchemasStatusResponse'
    { _drssrsRefreshSchemasStatus = Nothing
    , _drssrsResponseStatus = pResponseStatus_
    }


-- | The status of the schema.
drssrsRefreshSchemasStatus :: Lens' DescribeRefreshSchemasStatusResponse (Maybe RefreshSchemasStatus)
drssrsRefreshSchemasStatus = lens _drssrsRefreshSchemasStatus (\ s a -> s{_drssrsRefreshSchemasStatus = a})

-- | -- | The response status code.
drssrsResponseStatus :: Lens' DescribeRefreshSchemasStatusResponse Int
drssrsResponseStatus = lens _drssrsResponseStatus (\ s a -> s{_drssrsResponseStatus = a})

instance NFData DescribeRefreshSchemasStatusResponse
         where
