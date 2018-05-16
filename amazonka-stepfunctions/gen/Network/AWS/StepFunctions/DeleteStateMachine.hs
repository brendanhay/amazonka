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
-- Module      : Network.AWS.StepFunctions.DeleteStateMachine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a state machine. This is an asynchronous operation: It sets the state machine's status to @DELETING@ and begins the deletion process. Each state machine execution is deleted the next time it makes a state transition.
--
--
module Network.AWS.StepFunctions.DeleteStateMachine
    (
    -- * Creating a Request
      deleteStateMachine
    , DeleteStateMachine
    -- * Request Lenses
    , dStateMachineARN

    -- * Destructuring the Response
    , deleteStateMachineResponse
    , DeleteStateMachineResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'deleteStateMachine' smart constructor.
newtype DeleteStateMachine = DeleteStateMachine'
  { _dStateMachineARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStateMachine' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStateMachineARN' - The Amazon Resource Name (ARN) of the state machine to delete.
deleteStateMachine
    :: Text -- ^ 'dStateMachineARN'
    -> DeleteStateMachine
deleteStateMachine pStateMachineARN_ =
  DeleteStateMachine' {_dStateMachineARN = pStateMachineARN_}


-- | The Amazon Resource Name (ARN) of the state machine to delete.
dStateMachineARN :: Lens' DeleteStateMachine Text
dStateMachineARN = lens _dStateMachineARN (\ s a -> s{_dStateMachineARN = a})

instance AWSRequest DeleteStateMachine where
        type Rs DeleteStateMachine =
             DeleteStateMachineResponse
        request = postJSON stepFunctions
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteStateMachineResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteStateMachine where

instance NFData DeleteStateMachine where

instance ToHeaders DeleteStateMachine where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.DeleteStateMachine" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeleteStateMachine where
        toJSON DeleteStateMachine'{..}
          = object
              (catMaybes
                 [Just ("stateMachineArn" .= _dStateMachineARN)])

instance ToPath DeleteStateMachine where
        toPath = const "/"

instance ToQuery DeleteStateMachine where
        toQuery = const mempty

-- | /See:/ 'deleteStateMachineResponse' smart constructor.
newtype DeleteStateMachineResponse = DeleteStateMachineResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStateMachineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteStateMachineResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteStateMachineResponse
deleteStateMachineResponse pResponseStatus_ =
  DeleteStateMachineResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteStateMachineResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteStateMachineResponse where
