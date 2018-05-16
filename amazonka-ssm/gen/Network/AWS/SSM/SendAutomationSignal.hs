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
-- Module      : Network.AWS.SSM.SendAutomationSignal
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a signal to an Automation execution to change the current behavior or status of the execution.
--
--
module Network.AWS.SSM.SendAutomationSignal
    (
    -- * Creating a Request
      sendAutomationSignal
    , SendAutomationSignal
    -- * Request Lenses
    , sasPayload
    , sasAutomationExecutionId
    , sasSignalType

    -- * Destructuring the Response
    , sendAutomationSignalResponse
    , SendAutomationSignalResponse
    -- * Response Lenses
    , sasrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'sendAutomationSignal' smart constructor.
data SendAutomationSignal = SendAutomationSignal'
  { _sasPayload               :: !(Maybe (Map Text [Text]))
  , _sasAutomationExecutionId :: !Text
  , _sasSignalType            :: !SignalType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendAutomationSignal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasPayload' - The data sent with the signal. The data schema depends on the type of signal used in the request.
--
-- * 'sasAutomationExecutionId' - The unique identifier for an existing Automation execution that you want to send the signal to.
--
-- * 'sasSignalType' - The type of signal. Valid signal types include the following: Approve and Reject
sendAutomationSignal
    :: Text -- ^ 'sasAutomationExecutionId'
    -> SignalType -- ^ 'sasSignalType'
    -> SendAutomationSignal
sendAutomationSignal pAutomationExecutionId_ pSignalType_ =
  SendAutomationSignal'
    { _sasPayload = Nothing
    , _sasAutomationExecutionId = pAutomationExecutionId_
    , _sasSignalType = pSignalType_
    }


-- | The data sent with the signal. The data schema depends on the type of signal used in the request.
sasPayload :: Lens' SendAutomationSignal (HashMap Text [Text])
sasPayload = lens _sasPayload (\ s a -> s{_sasPayload = a}) . _Default . _Map

-- | The unique identifier for an existing Automation execution that you want to send the signal to.
sasAutomationExecutionId :: Lens' SendAutomationSignal Text
sasAutomationExecutionId = lens _sasAutomationExecutionId (\ s a -> s{_sasAutomationExecutionId = a})

-- | The type of signal. Valid signal types include the following: Approve and Reject
sasSignalType :: Lens' SendAutomationSignal SignalType
sasSignalType = lens _sasSignalType (\ s a -> s{_sasSignalType = a})

instance AWSRequest SendAutomationSignal where
        type Rs SendAutomationSignal =
             SendAutomationSignalResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 SendAutomationSignalResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SendAutomationSignal where

instance NFData SendAutomationSignal where

instance ToHeaders SendAutomationSignal where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.SendAutomationSignal" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SendAutomationSignal where
        toJSON SendAutomationSignal'{..}
          = object
              (catMaybes
                 [("Payload" .=) <$> _sasPayload,
                  Just
                    ("AutomationExecutionId" .=
                       _sasAutomationExecutionId),
                  Just ("SignalType" .= _sasSignalType)])

instance ToPath SendAutomationSignal where
        toPath = const "/"

instance ToQuery SendAutomationSignal where
        toQuery = const mempty

-- | /See:/ 'sendAutomationSignalResponse' smart constructor.
newtype SendAutomationSignalResponse = SendAutomationSignalResponse'
  { _sasrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendAutomationSignalResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sasrsResponseStatus' - -- | The response status code.
sendAutomationSignalResponse
    :: Int -- ^ 'sasrsResponseStatus'
    -> SendAutomationSignalResponse
sendAutomationSignalResponse pResponseStatus_ =
  SendAutomationSignalResponse' {_sasrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sasrsResponseStatus :: Lens' SendAutomationSignalResponse Int
sasrsResponseStatus = lens _sasrsResponseStatus (\ s a -> s{_sasrsResponseStatus = a})

instance NFData SendAutomationSignalResponse where
