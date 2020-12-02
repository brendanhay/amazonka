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
-- Module      : Network.AWS.DynamoDB.DescribeGlobalTableSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes region specific settings for a global table.
--
--
module Network.AWS.DynamoDB.DescribeGlobalTableSettings
    (
    -- * Creating a Request
      describeGlobalTableSettings
    , DescribeGlobalTableSettings
    -- * Request Lenses
    , dgtsGlobalTableName

    -- * Destructuring the Response
    , describeGlobalTableSettingsResponse
    , DescribeGlobalTableSettingsResponse
    -- * Response Lenses
    , dgtsrsReplicaSettings
    , dgtsrsGlobalTableName
    , dgtsrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeGlobalTableSettings' smart constructor.
newtype DescribeGlobalTableSettings = DescribeGlobalTableSettings'
  { _dgtsGlobalTableName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGlobalTableSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgtsGlobalTableName' - The name of the global table to describe.
describeGlobalTableSettings
    :: Text -- ^ 'dgtsGlobalTableName'
    -> DescribeGlobalTableSettings
describeGlobalTableSettings pGlobalTableName_ =
  DescribeGlobalTableSettings' {_dgtsGlobalTableName = pGlobalTableName_}


-- | The name of the global table to describe.
dgtsGlobalTableName :: Lens' DescribeGlobalTableSettings Text
dgtsGlobalTableName = lens _dgtsGlobalTableName (\ s a -> s{_dgtsGlobalTableName = a})

instance AWSRequest DescribeGlobalTableSettings where
        type Rs DescribeGlobalTableSettings =
             DescribeGlobalTableSettingsResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGlobalTableSettingsResponse' <$>
                   (x .?> "ReplicaSettings" .!@ mempty) <*>
                     (x .?> "GlobalTableName")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeGlobalTableSettings where

instance NFData DescribeGlobalTableSettings where

instance ToHeaders DescribeGlobalTableSettings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DescribeGlobalTableSettings" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeGlobalTableSettings where
        toJSON DescribeGlobalTableSettings'{..}
          = object
              (catMaybes
                 [Just ("GlobalTableName" .= _dgtsGlobalTableName)])

instance ToPath DescribeGlobalTableSettings where
        toPath = const "/"

instance ToQuery DescribeGlobalTableSettings where
        toQuery = const mempty

-- | /See:/ 'describeGlobalTableSettingsResponse' smart constructor.
data DescribeGlobalTableSettingsResponse = DescribeGlobalTableSettingsResponse'
  { _dgtsrsReplicaSettings :: !(Maybe [ReplicaSettingsDescription])
  , _dgtsrsGlobalTableName :: !(Maybe Text)
  , _dgtsrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGlobalTableSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgtsrsReplicaSettings' - The region specific settings for the global table.
--
-- * 'dgtsrsGlobalTableName' - The name of the global table.
--
-- * 'dgtsrsResponseStatus' - -- | The response status code.
describeGlobalTableSettingsResponse
    :: Int -- ^ 'dgtsrsResponseStatus'
    -> DescribeGlobalTableSettingsResponse
describeGlobalTableSettingsResponse pResponseStatus_ =
  DescribeGlobalTableSettingsResponse'
    { _dgtsrsReplicaSettings = Nothing
    , _dgtsrsGlobalTableName = Nothing
    , _dgtsrsResponseStatus = pResponseStatus_
    }


-- | The region specific settings for the global table.
dgtsrsReplicaSettings :: Lens' DescribeGlobalTableSettingsResponse [ReplicaSettingsDescription]
dgtsrsReplicaSettings = lens _dgtsrsReplicaSettings (\ s a -> s{_dgtsrsReplicaSettings = a}) . _Default . _Coerce

-- | The name of the global table.
dgtsrsGlobalTableName :: Lens' DescribeGlobalTableSettingsResponse (Maybe Text)
dgtsrsGlobalTableName = lens _dgtsrsGlobalTableName (\ s a -> s{_dgtsrsGlobalTableName = a})

-- | -- | The response status code.
dgtsrsResponseStatus :: Lens' DescribeGlobalTableSettingsResponse Int
dgtsrsResponseStatus = lens _dgtsrsResponseStatus (\ s a -> s{_dgtsrsResponseStatus = a})

instance NFData DescribeGlobalTableSettingsResponse
         where
