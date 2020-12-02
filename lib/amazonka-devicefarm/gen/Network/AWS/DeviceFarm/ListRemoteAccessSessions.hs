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
-- Module      : Network.AWS.DeviceFarm.ListRemoteAccessSessions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all currently running remote access sessions.
--
--
module Network.AWS.DeviceFarm.ListRemoteAccessSessions
    (
    -- * Creating a Request
      listRemoteAccessSessions
    , ListRemoteAccessSessions
    -- * Request Lenses
    , lrasNextToken
    , lrasArn

    -- * Destructuring the Response
    , listRemoteAccessSessionsResponse
    , ListRemoteAccessSessionsResponse
    -- * Response Lenses
    , lrasrsNextToken
    , lrasrsRemoteAccessSessions
    , lrasrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to return information about the remote access session.
--
--
--
-- /See:/ 'listRemoteAccessSessions' smart constructor.
data ListRemoteAccessSessions = ListRemoteAccessSessions'
  { _lrasNextToken :: !(Maybe Text)
  , _lrasArn       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRemoteAccessSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrasNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lrasArn' - The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
listRemoteAccessSessions
    :: Text -- ^ 'lrasArn'
    -> ListRemoteAccessSessions
listRemoteAccessSessions pArn_ =
  ListRemoteAccessSessions' {_lrasNextToken = Nothing, _lrasArn = pArn_}


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lrasNextToken :: Lens' ListRemoteAccessSessions (Maybe Text)
lrasNextToken = lens _lrasNextToken (\ s a -> s{_lrasNextToken = a})

-- | The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
lrasArn :: Lens' ListRemoteAccessSessions Text
lrasArn = lens _lrasArn (\ s a -> s{_lrasArn = a})

instance AWSRequest ListRemoteAccessSessions where
        type Rs ListRemoteAccessSessions =
             ListRemoteAccessSessionsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 ListRemoteAccessSessionsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "remoteAccessSessions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListRemoteAccessSessions where

instance NFData ListRemoteAccessSessions where

instance ToHeaders ListRemoteAccessSessions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListRemoteAccessSessions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRemoteAccessSessions where
        toJSON ListRemoteAccessSessions'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lrasNextToken,
                  Just ("arn" .= _lrasArn)])

instance ToPath ListRemoteAccessSessions where
        toPath = const "/"

instance ToQuery ListRemoteAccessSessions where
        toQuery = const mempty

-- | Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.
--
--
--
-- /See:/ 'listRemoteAccessSessionsResponse' smart constructor.
data ListRemoteAccessSessionsResponse = ListRemoteAccessSessionsResponse'
  { _lrasrsNextToken            :: !(Maybe Text)
  , _lrasrsRemoteAccessSessions :: !(Maybe [RemoteAccessSession])
  , _lrasrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRemoteAccessSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrasrsNextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- * 'lrasrsRemoteAccessSessions' - A container representing the metadata from the service about each remote access session you are requesting.
--
-- * 'lrasrsResponseStatus' - -- | The response status code.
listRemoteAccessSessionsResponse
    :: Int -- ^ 'lrasrsResponseStatus'
    -> ListRemoteAccessSessionsResponse
listRemoteAccessSessionsResponse pResponseStatus_ =
  ListRemoteAccessSessionsResponse'
    { _lrasrsNextToken = Nothing
    , _lrasrsRemoteAccessSessions = Nothing
    , _lrasrsResponseStatus = pResponseStatus_
    }


-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
lrasrsNextToken :: Lens' ListRemoteAccessSessionsResponse (Maybe Text)
lrasrsNextToken = lens _lrasrsNextToken (\ s a -> s{_lrasrsNextToken = a})

-- | A container representing the metadata from the service about each remote access session you are requesting.
lrasrsRemoteAccessSessions :: Lens' ListRemoteAccessSessionsResponse [RemoteAccessSession]
lrasrsRemoteAccessSessions = lens _lrasrsRemoteAccessSessions (\ s a -> s{_lrasrsRemoteAccessSessions = a}) . _Default . _Coerce

-- | -- | The response status code.
lrasrsResponseStatus :: Lens' ListRemoteAccessSessionsResponse Int
lrasrsResponseStatus = lens _lrasrsResponseStatus (\ s a -> s{_lrasrsResponseStatus = a})

instance NFData ListRemoteAccessSessionsResponse
         where
