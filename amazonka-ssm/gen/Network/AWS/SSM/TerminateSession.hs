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
-- Module      : Network.AWS.SSM.TerminateSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently ends a session and closes the data connection between the Session Manager client and SSM Agent on the instance. A terminated session cannot be resumed.
--
--
module Network.AWS.SSM.TerminateSession
    (
    -- * Creating a Request
      terminateSession
    , TerminateSession
    -- * Request Lenses
    , tsSessionId

    -- * Destructuring the Response
    , terminateSessionResponse
    , TerminateSessionResponse
    -- * Response Lenses
    , tsrsSessionId
    , tsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'terminateSession' smart constructor.
newtype TerminateSession = TerminateSession'
  { _tsSessionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsSessionId' - The ID of the session to terminate.
terminateSession
    :: Text -- ^ 'tsSessionId'
    -> TerminateSession
terminateSession pSessionId_ = TerminateSession' {_tsSessionId = pSessionId_}


-- | The ID of the session to terminate.
tsSessionId :: Lens' TerminateSession Text
tsSessionId = lens _tsSessionId (\ s a -> s{_tsSessionId = a})

instance AWSRequest TerminateSession where
        type Rs TerminateSession = TerminateSessionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 TerminateSessionResponse' <$>
                   (x .?> "SessionId") <*> (pure (fromEnum s)))

instance Hashable TerminateSession where

instance NFData TerminateSession where

instance ToHeaders TerminateSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.TerminateSession" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TerminateSession where
        toJSON TerminateSession'{..}
          = object
              (catMaybes [Just ("SessionId" .= _tsSessionId)])

instance ToPath TerminateSession where
        toPath = const "/"

instance ToQuery TerminateSession where
        toQuery = const mempty

-- | /See:/ 'terminateSessionResponse' smart constructor.
data TerminateSessionResponse = TerminateSessionResponse'
  { _tsrsSessionId      :: !(Maybe Text)
  , _tsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TerminateSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsrsSessionId' - The ID of the session that has been terminated.
--
-- * 'tsrsResponseStatus' - -- | The response status code.
terminateSessionResponse
    :: Int -- ^ 'tsrsResponseStatus'
    -> TerminateSessionResponse
terminateSessionResponse pResponseStatus_ =
  TerminateSessionResponse'
    {_tsrsSessionId = Nothing, _tsrsResponseStatus = pResponseStatus_}


-- | The ID of the session that has been terminated.
tsrsSessionId :: Lens' TerminateSessionResponse (Maybe Text)
tsrsSessionId = lens _tsrsSessionId (\ s a -> s{_tsrsSessionId = a})

-- | -- | The response status code.
tsrsResponseStatus :: Lens' TerminateSessionResponse Int
tsrsResponseStatus = lens _tsrsResponseStatus (\ s a -> s{_tsrsResponseStatus = a})

instance NFData TerminateSessionResponse where
