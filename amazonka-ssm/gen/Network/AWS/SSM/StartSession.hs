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
-- Module      : Network.AWS.SSM.StartSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a connection to a target (for example, an instance) for a Session Manager session. Returns a URL and token that can be used to open a WebSocket connection for sending input and receiving outputs.
--
--
module Network.AWS.SSM.StartSession
    (
    -- * Creating a Request
      startSession
    , StartSession
    -- * Request Lenses
    , ssDocumentName
    , ssParameters
    , ssTarget

    -- * Destructuring the Response
    , startSessionResponse
    , StartSessionResponse
    -- * Response Lenses
    , ssrsStreamURL
    , ssrsTokenValue
    , ssrsSessionId
    , ssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'startSession' smart constructor.
data StartSession = StartSession'
  { _ssDocumentName :: !(Maybe Text)
  , _ssParameters   :: !(Maybe (Map Text [Text]))
  , _ssTarget       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssDocumentName' - The name of the SSM document to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ . If no document name is provided, a shell to the instance is launched by default.
--
-- * 'ssParameters' - Reserved for future use.
--
-- * 'ssTarget' - The instance to connect to for the session.
startSession
    :: Text -- ^ 'ssTarget'
    -> StartSession
startSession pTarget_ =
  StartSession'
    {_ssDocumentName = Nothing, _ssParameters = Nothing, _ssTarget = pTarget_}


-- | The name of the SSM document to define the parameters and plugin settings for the session. For example, @SSM-SessionManagerRunShell@ . If no document name is provided, a shell to the instance is launched by default.
ssDocumentName :: Lens' StartSession (Maybe Text)
ssDocumentName = lens _ssDocumentName (\ s a -> s{_ssDocumentName = a})

-- | Reserved for future use.
ssParameters :: Lens' StartSession (HashMap Text [Text])
ssParameters = lens _ssParameters (\ s a -> s{_ssParameters = a}) . _Default . _Map

-- | The instance to connect to for the session.
ssTarget :: Lens' StartSession Text
ssTarget = lens _ssTarget (\ s a -> s{_ssTarget = a})

instance AWSRequest StartSession where
        type Rs StartSession = StartSessionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 StartSessionResponse' <$>
                   (x .?> "StreamUrl") <*> (x .?> "TokenValue") <*>
                     (x .?> "SessionId")
                     <*> (pure (fromEnum s)))

instance Hashable StartSession where

instance NFData StartSession where

instance ToHeaders StartSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.StartSession" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartSession where
        toJSON StartSession'{..}
          = object
              (catMaybes
                 [("DocumentName" .=) <$> _ssDocumentName,
                  ("Parameters" .=) <$> _ssParameters,
                  Just ("Target" .= _ssTarget)])

instance ToPath StartSession where
        toPath = const "/"

instance ToQuery StartSession where
        toQuery = const mempty

-- | /See:/ 'startSessionResponse' smart constructor.
data StartSessionResponse = StartSessionResponse'
  { _ssrsStreamURL      :: !(Maybe Text)
  , _ssrsTokenValue     :: !(Maybe Text)
  , _ssrsSessionId      :: !(Maybe Text)
  , _ssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssrsStreamURL' - A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssm-messages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@  __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in the <http://docs.aws.amazon.com/general/latest/gr/rande.html#ssm_region AWS Systems Manager table of regions and endpoints> in the /AWS General Reference/ . __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
--
-- * 'ssrsTokenValue' - An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
--
-- * 'ssrsSessionId' - The ID of the session.
--
-- * 'ssrsResponseStatus' - -- | The response status code.
startSessionResponse
    :: Int -- ^ 'ssrsResponseStatus'
    -> StartSessionResponse
startSessionResponse pResponseStatus_ =
  StartSessionResponse'
    { _ssrsStreamURL = Nothing
    , _ssrsTokenValue = Nothing
    , _ssrsSessionId = Nothing
    , _ssrsResponseStatus = pResponseStatus_
    }


-- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssm-messages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@  __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in the <http://docs.aws.amazon.com/general/latest/gr/rande.html#ssm_region AWS Systems Manager table of regions and endpoints> in the /AWS General Reference/ . __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
ssrsStreamURL :: Lens' StartSessionResponse (Maybe Text)
ssrsStreamURL = lens _ssrsStreamURL (\ s a -> s{_ssrsStreamURL = a})

-- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
ssrsTokenValue :: Lens' StartSessionResponse (Maybe Text)
ssrsTokenValue = lens _ssrsTokenValue (\ s a -> s{_ssrsTokenValue = a})

-- | The ID of the session.
ssrsSessionId :: Lens' StartSessionResponse (Maybe Text)
ssrsSessionId = lens _ssrsSessionId (\ s a -> s{_ssrsSessionId = a})

-- | -- | The response status code.
ssrsResponseStatus :: Lens' StartSessionResponse Int
ssrsResponseStatus = lens _ssrsResponseStatus (\ s a -> s{_ssrsResponseStatus = a})

instance NFData StartSessionResponse where
