{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetCognitoEvents
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the events and the corresponding Lambda functions associated with
-- an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetCognitoEvents.html>
module Network.AWS.CognitoSync.GetCognitoEvents
    (
    -- * Request
      GetCognitoEvents
    -- ** Request constructor
    , getCognitoEvents
    -- ** Request lenses
    , gceIdentityPoolId

    -- * Response
    , GetCognitoEventsResponse
    -- ** Response constructor
    , getCognitoEventsResponse
    -- ** Response lenses
    , gcersEvents
    , gcersStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request for a list of the configured Cognito Events
--
-- /See:/ 'getCognitoEvents' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gceIdentityPoolId'
newtype GetCognitoEvents = GetCognitoEvents'
    { _gceIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCognitoEvents' smart constructor.
getCognitoEvents :: Text -> GetCognitoEvents
getCognitoEvents pIdentityPoolId_ =
    GetCognitoEvents'
    { _gceIdentityPoolId = pIdentityPoolId_
    }

-- | The Cognito Identity Pool ID for the request
gceIdentityPoolId :: Lens' GetCognitoEvents Text
gceIdentityPoolId = lens _gceIdentityPoolId (\ s a -> s{_gceIdentityPoolId = a});

instance AWSRequest GetCognitoEvents where
        type Sv GetCognitoEvents = CognitoSync
        type Rs GetCognitoEvents = GetCognitoEventsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetCognitoEventsResponse' <$>
                   (x .?> "Events" .!@ mempty) <*> (pure (fromEnum s)))

instance ToHeaders GetCognitoEvents where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCognitoEvents where
        toPath GetCognitoEvents'{..}
          = mconcat
              ["/identitypools/", toText _gceIdentityPoolId,
               "/events"]

instance ToQuery GetCognitoEvents where
        toQuery = const mempty

-- | The response from the GetCognitoEvents request
--
-- /See:/ 'getCognitoEventsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gcersEvents'
--
-- * 'gcersStatus'
data GetCognitoEventsResponse = GetCognitoEventsResponse'
    { _gcersEvents :: !(Maybe (Map Text Text))
    , _gcersStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetCognitoEventsResponse' smart constructor.
getCognitoEventsResponse :: Int -> GetCognitoEventsResponse
getCognitoEventsResponse pStatus_ =
    GetCognitoEventsResponse'
    { _gcersEvents = Nothing
    , _gcersStatus = pStatus_
    }

-- | The Cognito Events returned from the GetCognitoEvents request
gcersEvents :: Lens' GetCognitoEventsResponse (HashMap Text Text)
gcersEvents = lens _gcersEvents (\ s a -> s{_gcersEvents = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
gcersStatus :: Lens' GetCognitoEventsResponse Int
gcersStatus = lens _gcersStatus (\ s a -> s{_gcersStatus = a});
