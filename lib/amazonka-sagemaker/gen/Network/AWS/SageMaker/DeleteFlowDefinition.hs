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
-- Module      : Network.AWS.SageMaker.DeleteFlowDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified flow definition.
module Network.AWS.SageMaker.DeleteFlowDefinition
  ( -- * Creating a Request
    deleteFlowDefinition,
    DeleteFlowDefinition,

    -- * Request Lenses
    dfdFlowDefinitionName,

    -- * Destructuring the Response
    deleteFlowDefinitionResponse,
    DeleteFlowDefinitionResponse,

    -- * Response Lenses
    dfdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteFlowDefinition' smart constructor.
newtype DeleteFlowDefinition = DeleteFlowDefinition'
  { _dfdFlowDefinitionName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFlowDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfdFlowDefinitionName' - The name of the flow definition you are deleting.
deleteFlowDefinition ::
  -- | 'dfdFlowDefinitionName'
  Text ->
  DeleteFlowDefinition
deleteFlowDefinition pFlowDefinitionName_ =
  DeleteFlowDefinition'
    { _dfdFlowDefinitionName =
        pFlowDefinitionName_
    }

-- | The name of the flow definition you are deleting.
dfdFlowDefinitionName :: Lens' DeleteFlowDefinition Text
dfdFlowDefinitionName = lens _dfdFlowDefinitionName (\s a -> s {_dfdFlowDefinitionName = a})

instance AWSRequest DeleteFlowDefinition where
  type Rs DeleteFlowDefinition = DeleteFlowDefinitionResponse
  request = postJSON sageMaker
  response =
    receiveEmpty
      (\s h x -> DeleteFlowDefinitionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteFlowDefinition

instance NFData DeleteFlowDefinition

instance ToHeaders DeleteFlowDefinition where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DeleteFlowDefinition" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteFlowDefinition where
  toJSON DeleteFlowDefinition' {..} =
    object
      (catMaybes [Just ("FlowDefinitionName" .= _dfdFlowDefinitionName)])

instance ToPath DeleteFlowDefinition where
  toPath = const "/"

instance ToQuery DeleteFlowDefinition where
  toQuery = const mempty

-- | /See:/ 'deleteFlowDefinitionResponse' smart constructor.
newtype DeleteFlowDefinitionResponse = DeleteFlowDefinitionResponse'
  { _dfdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFlowDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfdrsResponseStatus' - -- | The response status code.
deleteFlowDefinitionResponse ::
  -- | 'dfdrsResponseStatus'
  Int ->
  DeleteFlowDefinitionResponse
deleteFlowDefinitionResponse pResponseStatus_ =
  DeleteFlowDefinitionResponse'
    { _dfdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dfdrsResponseStatus :: Lens' DeleteFlowDefinitionResponse Int
dfdrsResponseStatus = lens _dfdrsResponseStatus (\s a -> s {_dfdrsResponseStatus = a})

instance NFData DeleteFlowDefinitionResponse
