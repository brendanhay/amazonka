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
-- Module      : Network.AWS.StepFunctions.CreateStateMachine
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a state machine. A state machine consists of a collection of states that can do work (@Task@ states), determine to which states to transition next (@Choice@ states), stop an execution with an error (@Fail@ states), and so on. State machines are specified using a JSON-based, structured language.
--
--
module Network.AWS.StepFunctions.CreateStateMachine
    (
    -- * Creating a Request
      createStateMachine
    , CreateStateMachine
    -- * Request Lenses
    , csmTags
    , csmName
    , csmDefinition
    , csmRoleARN

    -- * Destructuring the Response
    , createStateMachineResponse
    , CreateStateMachineResponse
    -- * Response Lenses
    , csmrsResponseStatus
    , csmrsStateMachineARN
    , csmrsCreationDate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'createStateMachine' smart constructor.
data CreateStateMachine = CreateStateMachine'
  { _csmTags       :: !(Maybe [Tag])
  , _csmName       :: !Text
  , _csmDefinition :: !(Sensitive Text)
  , _csmRoleARN    :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStateMachine' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmTags' - Tags to be added when creating a state machine.
--
-- * 'csmName' - The name of the state machine.  A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
-- * 'csmDefinition' - The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
--
-- * 'csmRoleARN' - The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
createStateMachine
    :: Text -- ^ 'csmName'
    -> Text -- ^ 'csmDefinition'
    -> Text -- ^ 'csmRoleARN'
    -> CreateStateMachine
createStateMachine pName_ pDefinition_ pRoleARN_ =
  CreateStateMachine'
    { _csmTags = Nothing
    , _csmName = pName_
    , _csmDefinition = _Sensitive # pDefinition_
    , _csmRoleARN = pRoleARN_
    }


-- | Tags to be added when creating a state machine.
csmTags :: Lens' CreateStateMachine [Tag]
csmTags = lens _csmTags (\ s a -> s{_csmTags = a}) . _Default . _Coerce

-- | The name of the state machine.  A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
csmName :: Lens' CreateStateMachine Text
csmName = lens _csmName (\ s a -> s{_csmName = a})

-- | The Amazon States Language definition of the state machine. See <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-amazon-states-language.html Amazon States Language> .
csmDefinition :: Lens' CreateStateMachine Text
csmDefinition = lens _csmDefinition (\ s a -> s{_csmDefinition = a}) . _Sensitive

-- | The Amazon Resource Name (ARN) of the IAM role to use for this state machine.
csmRoleARN :: Lens' CreateStateMachine Text
csmRoleARN = lens _csmRoleARN (\ s a -> s{_csmRoleARN = a})

instance AWSRequest CreateStateMachine where
        type Rs CreateStateMachine =
             CreateStateMachineResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 CreateStateMachineResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "stateMachineArn") <*>
                     (x .:> "creationDate"))

instance Hashable CreateStateMachine where

instance NFData CreateStateMachine where

instance ToHeaders CreateStateMachine where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.CreateStateMachine" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CreateStateMachine where
        toJSON CreateStateMachine'{..}
          = object
              (catMaybes
                 [("tags" .=) <$> _csmTags, Just ("name" .= _csmName),
                  Just ("definition" .= _csmDefinition),
                  Just ("roleArn" .= _csmRoleARN)])

instance ToPath CreateStateMachine where
        toPath = const "/"

instance ToQuery CreateStateMachine where
        toQuery = const mempty

-- | /See:/ 'createStateMachineResponse' smart constructor.
data CreateStateMachineResponse = CreateStateMachineResponse'
  { _csmrsResponseStatus  :: !Int
  , _csmrsStateMachineARN :: !Text
  , _csmrsCreationDate    :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateStateMachineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csmrsResponseStatus' - -- | The response status code.
--
-- * 'csmrsStateMachineARN' - The Amazon Resource Name (ARN) that identifies the created state machine.
--
-- * 'csmrsCreationDate' - The date the state machine is created.
createStateMachineResponse
    :: Int -- ^ 'csmrsResponseStatus'
    -> Text -- ^ 'csmrsStateMachineARN'
    -> UTCTime -- ^ 'csmrsCreationDate'
    -> CreateStateMachineResponse
createStateMachineResponse pResponseStatus_ pStateMachineARN_ pCreationDate_ =
  CreateStateMachineResponse'
    { _csmrsResponseStatus = pResponseStatus_
    , _csmrsStateMachineARN = pStateMachineARN_
    , _csmrsCreationDate = _Time # pCreationDate_
    }


-- | -- | The response status code.
csmrsResponseStatus :: Lens' CreateStateMachineResponse Int
csmrsResponseStatus = lens _csmrsResponseStatus (\ s a -> s{_csmrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the created state machine.
csmrsStateMachineARN :: Lens' CreateStateMachineResponse Text
csmrsStateMachineARN = lens _csmrsStateMachineARN (\ s a -> s{_csmrsStateMachineARN = a})

-- | The date the state machine is created.
csmrsCreationDate :: Lens' CreateStateMachineResponse UTCTime
csmrsCreationDate = lens _csmrsCreationDate (\ s a -> s{_csmrsCreationDate = a}) . _Time

instance NFData CreateStateMachineResponse where
