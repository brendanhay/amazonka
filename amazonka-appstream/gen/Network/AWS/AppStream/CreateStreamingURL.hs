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
-- Module      : Network.AWS.AppStream.CreateStreamingURL
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL to start an AppStream 2.0 streaming session for a user. By default, the URL is valid only for 1 minute from the time that it is generated.
--
--
module Network.AWS.AppStream.CreateStreamingURL
    (
    -- * Creating a Request
      createStreamingURL
    , CreateStreamingURL
    -- * Request Lenses
    , csuSessionContext
    , csuApplicationId
    , csuValidity
    , csuStackName
    , csuFleetName
    , csuUserId

    -- * Destructuring the Response
    , createStreamingURLResponse
    , CreateStreamingURLResponse
    -- * Response Lenses
    , csursStreamingURL
    , csursExpires
    , csursResponseStatus
    ) where

import           Network.AWS.AppStream.Types
import           Network.AWS.AppStream.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createStreamingURL' smart constructor.
data CreateStreamingURL = CreateStreamingURL'
    { _csuSessionContext :: !(Maybe Text)
    , _csuApplicationId  :: !(Maybe Text)
    , _csuValidity       :: !(Maybe Integer)
    , _csuStackName      :: !Text
    , _csuFleetName      :: !Text
    , _csuUserId         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStreamingURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csuSessionContext' - The sessionContext of the streaming URL.
--
-- * 'csuApplicationId' - The ID of the application that must be launched after the session starts.
--
-- * 'csuValidity' - The duration up to which the URL returned by this action is valid. The input can be any numeric value in seconds between 1 and 604800 seconds.
--
-- * 'csuStackName' - The stack for which the URL is generated.
--
-- * 'csuFleetName' - The fleet for which the URL is generated.
--
-- * 'csuUserId' - A unique user ID for whom the URL is generated.
createStreamingURL
    :: Text -- ^ 'csuStackName'
    -> Text -- ^ 'csuFleetName'
    -> Text -- ^ 'csuUserId'
    -> CreateStreamingURL
createStreamingURL pStackName_ pFleetName_ pUserId_ =
    CreateStreamingURL'
    { _csuSessionContext = Nothing
    , _csuApplicationId = Nothing
    , _csuValidity = Nothing
    , _csuStackName = pStackName_
    , _csuFleetName = pFleetName_
    , _csuUserId = pUserId_
    }

-- | The sessionContext of the streaming URL.
csuSessionContext :: Lens' CreateStreamingURL (Maybe Text)
csuSessionContext = lens _csuSessionContext (\ s a -> s{_csuSessionContext = a});

-- | The ID of the application that must be launched after the session starts.
csuApplicationId :: Lens' CreateStreamingURL (Maybe Text)
csuApplicationId = lens _csuApplicationId (\ s a -> s{_csuApplicationId = a});

-- | The duration up to which the URL returned by this action is valid. The input can be any numeric value in seconds between 1 and 604800 seconds.
csuValidity :: Lens' CreateStreamingURL (Maybe Integer)
csuValidity = lens _csuValidity (\ s a -> s{_csuValidity = a});

-- | The stack for which the URL is generated.
csuStackName :: Lens' CreateStreamingURL Text
csuStackName = lens _csuStackName (\ s a -> s{_csuStackName = a});

-- | The fleet for which the URL is generated.
csuFleetName :: Lens' CreateStreamingURL Text
csuFleetName = lens _csuFleetName (\ s a -> s{_csuFleetName = a});

-- | A unique user ID for whom the URL is generated.
csuUserId :: Lens' CreateStreamingURL Text
csuUserId = lens _csuUserId (\ s a -> s{_csuUserId = a});

instance AWSRequest CreateStreamingURL where
        type Rs CreateStreamingURL =
             CreateStreamingURLResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CreateStreamingURLResponse' <$>
                   (x .?> "StreamingURL") <*> (x .?> "Expires") <*>
                     (pure (fromEnum s)))

instance Hashable CreateStreamingURL

instance NFData CreateStreamingURL

instance ToHeaders CreateStreamingURL where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.CreateStreamingURL" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateStreamingURL where
        toJSON CreateStreamingURL'{..}
          = object
              (catMaybes
                 [("SessionContext" .=) <$> _csuSessionContext,
                  ("ApplicationId" .=) <$> _csuApplicationId,
                  ("Validity" .=) <$> _csuValidity,
                  Just ("StackName" .= _csuStackName),
                  Just ("FleetName" .= _csuFleetName),
                  Just ("UserId" .= _csuUserId)])

instance ToPath CreateStreamingURL where
        toPath = const "/"

instance ToQuery CreateStreamingURL where
        toQuery = const mempty

-- | /See:/ 'createStreamingURLResponse' smart constructor.
data CreateStreamingURLResponse = CreateStreamingURLResponse'
    { _csursStreamingURL   :: !(Maybe Text)
    , _csursExpires        :: !(Maybe POSIX)
    , _csursResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateStreamingURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csursStreamingURL' - The URL to start the AppStream 2.0 streaming session.
--
-- * 'csursExpires' - Elapsed seconds after the Unix epoch, when this URL expires.
--
-- * 'csursResponseStatus' - -- | The response status code.
createStreamingURLResponse
    :: Int -- ^ 'csursResponseStatus'
    -> CreateStreamingURLResponse
createStreamingURLResponse pResponseStatus_ =
    CreateStreamingURLResponse'
    { _csursStreamingURL = Nothing
    , _csursExpires = Nothing
    , _csursResponseStatus = pResponseStatus_
    }

-- | The URL to start the AppStream 2.0 streaming session.
csursStreamingURL :: Lens' CreateStreamingURLResponse (Maybe Text)
csursStreamingURL = lens _csursStreamingURL (\ s a -> s{_csursStreamingURL = a});

-- | Elapsed seconds after the Unix epoch, when this URL expires.
csursExpires :: Lens' CreateStreamingURLResponse (Maybe UTCTime)
csursExpires = lens _csursExpires (\ s a -> s{_csursExpires = a}) . mapping _Time;

-- | -- | The response status code.
csursResponseStatus :: Lens' CreateStreamingURLResponse Int
csursResponseStatus = lens _csursResponseStatus (\ s a -> s{_csursResponseStatus = a});

instance NFData CreateStreamingURLResponse
