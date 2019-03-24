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
-- Module      : Network.AWS.SSM.ResumeSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reconnects a session to an instance after it has been disconnected. Connections can be resumed for disconnected sessions, but not terminated sessions.
--
--
module Network.AWS.SSM.ResumeSession
    (
    -- * Creating a Request
      resumeSession
    , ResumeSession
    -- * Request Lenses
    , rsSessionId

    -- * Destructuring the Response
    , resumeSessionResponse
    , ResumeSessionResponse
    -- * Response Lenses
    , rsrsStreamURL
    , rsrsTokenValue
    , rsrsSessionId
    , rsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'resumeSession' smart constructor.
newtype ResumeSession = ResumeSession'
  { _rsSessionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResumeSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsSessionId' - The ID of the disconnected session to resume.
resumeSession
    :: Text -- ^ 'rsSessionId'
    -> ResumeSession
resumeSession pSessionId_ = ResumeSession' {_rsSessionId = pSessionId_}


-- | The ID of the disconnected session to resume.
rsSessionId :: Lens' ResumeSession Text
rsSessionId = lens _rsSessionId (\ s a -> s{_rsSessionId = a})

instance AWSRequest ResumeSession where
        type Rs ResumeSession = ResumeSessionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ResumeSessionResponse' <$>
                   (x .?> "StreamUrl") <*> (x .?> "TokenValue") <*>
                     (x .?> "SessionId")
                     <*> (pure (fromEnum s)))

instance Hashable ResumeSession where

instance NFData ResumeSession where

instance ToHeaders ResumeSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ResumeSession" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResumeSession where
        toJSON ResumeSession'{..}
          = object
              (catMaybes [Just ("SessionId" .= _rsSessionId)])

instance ToPath ResumeSession where
        toPath = const "/"

instance ToQuery ResumeSession where
        toQuery = const mempty

-- | /See:/ 'resumeSessionResponse' smart constructor.
data ResumeSessionResponse = ResumeSessionResponse'
  { _rsrsStreamURL      :: !(Maybe Text)
  , _rsrsTokenValue     :: !(Maybe Text)
  , _rsrsSessionId      :: !(Maybe Text)
  , _rsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResumeSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsrsStreamURL' - A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssm-messages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@ . __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in the <http://docs.aws.amazon.com/general/latest/gr/rande.html#ssm_region AWS Systems Manager table of regions and endpoints> in the /AWS General Reference/ . __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
--
-- * 'rsrsTokenValue' - An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
--
-- * 'rsrsSessionId' - The ID of the session.
--
-- * 'rsrsResponseStatus' - -- | The response status code.
resumeSessionResponse
    :: Int -- ^ 'rsrsResponseStatus'
    -> ResumeSessionResponse
resumeSessionResponse pResponseStatus_ =
  ResumeSessionResponse'
    { _rsrsStreamURL = Nothing
    , _rsrsTokenValue = Nothing
    , _rsrsSessionId = Nothing
    , _rsrsResponseStatus = pResponseStatus_
    }


-- | A URL back to SSM Agent on the instance that the Session Manager client uses to send commands and receive output from the instance. Format: @wss://ssm-messages.__region__ .amazonaws.com/v1/data-channel/__session-id__ ?stream=(input|output)@ . __region__ represents the Region identifier for an AWS Region supported by AWS Systems Manager, such as @us-east-2@ for the US East (Ohio) Region. For a list of supported __region__ values, see the __Region__ column in the <http://docs.aws.amazon.com/general/latest/gr/rande.html#ssm_region AWS Systems Manager table of regions and endpoints> in the /AWS General Reference/ . __session-id__ represents the ID of a Session Manager session, such as @1a2b3c4dEXAMPLE@ .
rsrsStreamURL :: Lens' ResumeSessionResponse (Maybe Text)
rsrsStreamURL = lens _rsrsStreamURL (\ s a -> s{_rsrsStreamURL = a})

-- | An encrypted token value containing session and caller information. Used to authenticate the connection to the instance.
rsrsTokenValue :: Lens' ResumeSessionResponse (Maybe Text)
rsrsTokenValue = lens _rsrsTokenValue (\ s a -> s{_rsrsTokenValue = a})

-- | The ID of the session.
rsrsSessionId :: Lens' ResumeSessionResponse (Maybe Text)
rsrsSessionId = lens _rsrsSessionId (\ s a -> s{_rsrsSessionId = a})

-- | -- | The response status code.
rsrsResponseStatus :: Lens' ResumeSessionResponse Int
rsrsResponseStatus = lens _rsrsResponseStatus (\ s a -> s{_rsrsResponseStatus = a})

instance NFData ResumeSessionResponse where
