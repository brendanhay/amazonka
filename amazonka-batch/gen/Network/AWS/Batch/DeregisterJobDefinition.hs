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
-- Module      : Network.AWS.Batch.DeregisterJobDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an AWS Batch job definition.
--
--
module Network.AWS.Batch.DeregisterJobDefinition
    (
    -- * Creating a Request
      deregisterJobDefinition
    , DeregisterJobDefinition
    -- * Request Lenses
    , djdJobDefinition

    -- * Destructuring the Response
    , deregisterJobDefinitionResponse
    , DeregisterJobDefinitionResponse
    -- * Response Lenses
    , derrsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterJobDefinition' smart constructor.
newtype DeregisterJobDefinition = DeregisterJobDefinition'
  { _djdJobDefinition :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djdJobDefinition' - The name and revision (@name:revision@ ) or full Amazon Resource Name (ARN) of the job definition to deregister.
deregisterJobDefinition
    :: Text -- ^ 'djdJobDefinition'
    -> DeregisterJobDefinition
deregisterJobDefinition pJobDefinition_ =
  DeregisterJobDefinition' {_djdJobDefinition = pJobDefinition_}


-- | The name and revision (@name:revision@ ) or full Amazon Resource Name (ARN) of the job definition to deregister.
djdJobDefinition :: Lens' DeregisterJobDefinition Text
djdJobDefinition = lens _djdJobDefinition (\ s a -> s{_djdJobDefinition = a})

instance AWSRequest DeregisterJobDefinition where
        type Rs DeregisterJobDefinition =
             DeregisterJobDefinitionResponse
        request = postJSON batch
        response
          = receiveEmpty
              (\ s h x ->
                 DeregisterJobDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeregisterJobDefinition where

instance NFData DeregisterJobDefinition where

instance ToHeaders DeregisterJobDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterJobDefinition where
        toJSON DeregisterJobDefinition'{..}
          = object
              (catMaybes
                 [Just ("jobDefinition" .= _djdJobDefinition)])

instance ToPath DeregisterJobDefinition where
        toPath = const "/v1/deregisterjobdefinition"

instance ToQuery DeregisterJobDefinition where
        toQuery = const mempty

-- | /See:/ 'deregisterJobDefinitionResponse' smart constructor.
newtype DeregisterJobDefinitionResponse = DeregisterJobDefinitionResponse'
  { _derrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeregisterJobDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derrsResponseStatus' - -- | The response status code.
deregisterJobDefinitionResponse
    :: Int -- ^ 'derrsResponseStatus'
    -> DeregisterJobDefinitionResponse
deregisterJobDefinitionResponse pResponseStatus_ =
  DeregisterJobDefinitionResponse' {_derrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
derrsResponseStatus :: Lens' DeregisterJobDefinitionResponse Int
derrsResponseStatus = lens _derrsResponseStatus (\ s a -> s{_derrsResponseStatus = a})

instance NFData DeregisterJobDefinitionResponse where
