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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the feedback for an authentication event whether it was from a valid user or not. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.
--
--
module Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback
    (
    -- * Creating a Request
      updateAuthEventFeedback
    , UpdateAuthEventFeedback
    -- * Request Lenses
    , uaefUserPoolId
    , uaefUsername
    , uaefEventId
    , uaefFeedbackToken
    , uaefFeedbackValue

    -- * Destructuring the Response
    , updateAuthEventFeedbackResponse
    , UpdateAuthEventFeedbackResponse
    -- * Response Lenses
    , uaefrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAuthEventFeedback' smart constructor.
data UpdateAuthEventFeedback = UpdateAuthEventFeedback'
  { _uaefUserPoolId    :: !Text
  , _uaefUsername      :: !(Sensitive Text)
  , _uaefEventId       :: !Text
  , _uaefFeedbackToken :: !(Sensitive Text)
  , _uaefFeedbackValue :: !FeedbackValueType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAuthEventFeedback' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaefUserPoolId' - The user pool ID.
--
-- * 'uaefUsername' - The user pool username.
--
-- * 'uaefEventId' - The event ID.
--
-- * 'uaefFeedbackToken' - The feedback token.
--
-- * 'uaefFeedbackValue' - The authentication event feedback value.
updateAuthEventFeedback
    :: Text -- ^ 'uaefUserPoolId'
    -> Text -- ^ 'uaefUsername'
    -> Text -- ^ 'uaefEventId'
    -> Text -- ^ 'uaefFeedbackToken'
    -> FeedbackValueType -- ^ 'uaefFeedbackValue'
    -> UpdateAuthEventFeedback
updateAuthEventFeedback pUserPoolId_ pUsername_ pEventId_ pFeedbackToken_ pFeedbackValue_ =
  UpdateAuthEventFeedback'
    { _uaefUserPoolId = pUserPoolId_
    , _uaefUsername = _Sensitive # pUsername_
    , _uaefEventId = pEventId_
    , _uaefFeedbackToken = _Sensitive # pFeedbackToken_
    , _uaefFeedbackValue = pFeedbackValue_
    }


-- | The user pool ID.
uaefUserPoolId :: Lens' UpdateAuthEventFeedback Text
uaefUserPoolId = lens _uaefUserPoolId (\ s a -> s{_uaefUserPoolId = a})

-- | The user pool username.
uaefUsername :: Lens' UpdateAuthEventFeedback Text
uaefUsername = lens _uaefUsername (\ s a -> s{_uaefUsername = a}) . _Sensitive

-- | The event ID.
uaefEventId :: Lens' UpdateAuthEventFeedback Text
uaefEventId = lens _uaefEventId (\ s a -> s{_uaefEventId = a})

-- | The feedback token.
uaefFeedbackToken :: Lens' UpdateAuthEventFeedback Text
uaefFeedbackToken = lens _uaefFeedbackToken (\ s a -> s{_uaefFeedbackToken = a}) . _Sensitive

-- | The authentication event feedback value.
uaefFeedbackValue :: Lens' UpdateAuthEventFeedback FeedbackValueType
uaefFeedbackValue = lens _uaefFeedbackValue (\ s a -> s{_uaefFeedbackValue = a})

instance AWSRequest UpdateAuthEventFeedback where
        type Rs UpdateAuthEventFeedback =
             UpdateAuthEventFeedbackResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateAuthEventFeedbackResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateAuthEventFeedback where

instance NFData UpdateAuthEventFeedback where

instance ToHeaders UpdateAuthEventFeedback where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateAuthEventFeedback"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAuthEventFeedback where
        toJSON UpdateAuthEventFeedback'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _uaefUserPoolId),
                  Just ("Username" .= _uaefUsername),
                  Just ("EventId" .= _uaefEventId),
                  Just ("FeedbackToken" .= _uaefFeedbackToken),
                  Just ("FeedbackValue" .= _uaefFeedbackValue)])

instance ToPath UpdateAuthEventFeedback where
        toPath = const "/"

instance ToQuery UpdateAuthEventFeedback where
        toQuery = const mempty

-- | /See:/ 'updateAuthEventFeedbackResponse' smart constructor.
newtype UpdateAuthEventFeedbackResponse = UpdateAuthEventFeedbackResponse'
  { _uaefrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAuthEventFeedbackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaefrsResponseStatus' - -- | The response status code.
updateAuthEventFeedbackResponse
    :: Int -- ^ 'uaefrsResponseStatus'
    -> UpdateAuthEventFeedbackResponse
updateAuthEventFeedbackResponse pResponseStatus_ =
  UpdateAuthEventFeedbackResponse' {_uaefrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uaefrsResponseStatus :: Lens' UpdateAuthEventFeedbackResponse Int
uaefrsResponseStatus = lens _uaefrsResponseStatus (\ s a -> s{_uaefrsResponseStatus = a})

instance NFData UpdateAuthEventFeedbackResponse where
