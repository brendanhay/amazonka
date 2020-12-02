{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic rule destination. The destination must be confirmed prior to use.
module Network.AWS.IoT.CreateTopicRuleDestination
  ( -- * Creating a Request
    createTopicRuleDestination,
    CreateTopicRuleDestination,

    -- * Request Lenses
    ctrdDestinationConfiguration,

    -- * Destructuring the Response
    createTopicRuleDestinationResponse,
    CreateTopicRuleDestinationResponse,

    -- * Response Lenses
    crsTopicRuleDestination,
    crsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTopicRuleDestination' smart constructor.
newtype CreateTopicRuleDestination = CreateTopicRuleDestination'
  { _ctrdDestinationConfiguration ::
      TopicRuleDestinationConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTopicRuleDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctrdDestinationConfiguration' - The topic rule destination configuration.
createTopicRuleDestination ::
  -- | 'ctrdDestinationConfiguration'
  TopicRuleDestinationConfiguration ->
  CreateTopicRuleDestination
createTopicRuleDestination pDestinationConfiguration_ =
  CreateTopicRuleDestination'
    { _ctrdDestinationConfiguration =
        pDestinationConfiguration_
    }

-- | The topic rule destination configuration.
ctrdDestinationConfiguration :: Lens' CreateTopicRuleDestination TopicRuleDestinationConfiguration
ctrdDestinationConfiguration = lens _ctrdDestinationConfiguration (\s a -> s {_ctrdDestinationConfiguration = a})

instance AWSRequest CreateTopicRuleDestination where
  type
    Rs CreateTopicRuleDestination =
      CreateTopicRuleDestinationResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateTopicRuleDestinationResponse'
            <$> (x .?> "topicRuleDestination") <*> (pure (fromEnum s))
      )

instance Hashable CreateTopicRuleDestination

instance NFData CreateTopicRuleDestination

instance ToHeaders CreateTopicRuleDestination where
  toHeaders = const mempty

instance ToJSON CreateTopicRuleDestination where
  toJSON CreateTopicRuleDestination' {..} =
    object
      ( catMaybes
          [ Just
              ("destinationConfiguration" .= _ctrdDestinationConfiguration)
          ]
      )

instance ToPath CreateTopicRuleDestination where
  toPath = const "/destinations"

instance ToQuery CreateTopicRuleDestination where
  toQuery = const mempty

-- | /See:/ 'createTopicRuleDestinationResponse' smart constructor.
data CreateTopicRuleDestinationResponse = CreateTopicRuleDestinationResponse'
  { _crsTopicRuleDestination ::
      !( Maybe
           TopicRuleDestination
       ),
    _crsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsTopicRuleDestination' - The topic rule destination.
--
-- * 'crsResponseStatus' - -- | The response status code.
createTopicRuleDestinationResponse ::
  -- | 'crsResponseStatus'
  Int ->
  CreateTopicRuleDestinationResponse
createTopicRuleDestinationResponse pResponseStatus_ =
  CreateTopicRuleDestinationResponse'
    { _crsTopicRuleDestination =
        Nothing,
      _crsResponseStatus = pResponseStatus_
    }

-- | The topic rule destination.
crsTopicRuleDestination :: Lens' CreateTopicRuleDestinationResponse (Maybe TopicRuleDestination)
crsTopicRuleDestination = lens _crsTopicRuleDestination (\s a -> s {_crsTopicRuleDestination = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateTopicRuleDestinationResponse Int
crsResponseStatus = lens _crsResponseStatus (\s a -> s {_crsResponseStatus = a})

instance NFData CreateTopicRuleDestinationResponse
