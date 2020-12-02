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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides feedback for an authentication event as to whether it was from a valid user. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.
--
--
module Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
    (
    -- * Creating a Request
      adminUpdateAuthEventFeedback
    , AdminUpdateAuthEventFeedback
    -- * Request Lenses
    , auaefUserPoolId
    , auaefUsername
    , auaefEventId
    , auaefFeedbackValue

    -- * Destructuring the Response
    , adminUpdateAuthEventFeedbackResponse
    , AdminUpdateAuthEventFeedbackResponse
    -- * Response Lenses
    , auaefrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'adminUpdateAuthEventFeedback' smart constructor.
data AdminUpdateAuthEventFeedback = AdminUpdateAuthEventFeedback'
  { _auaefUserPoolId    :: !Text
  , _auaefUsername      :: !(Sensitive Text)
  , _auaefEventId       :: !Text
  , _auaefFeedbackValue :: !FeedbackValueType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUpdateAuthEventFeedback' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auaefUserPoolId' - The user pool ID.
--
-- * 'auaefUsername' - The user pool username.
--
-- * 'auaefEventId' - The authentication event ID.
--
-- * 'auaefFeedbackValue' - The authentication event feedback value.
adminUpdateAuthEventFeedback
    :: Text -- ^ 'auaefUserPoolId'
    -> Text -- ^ 'auaefUsername'
    -> Text -- ^ 'auaefEventId'
    -> FeedbackValueType -- ^ 'auaefFeedbackValue'
    -> AdminUpdateAuthEventFeedback
adminUpdateAuthEventFeedback pUserPoolId_ pUsername_ pEventId_ pFeedbackValue_ =
  AdminUpdateAuthEventFeedback'
    { _auaefUserPoolId = pUserPoolId_
    , _auaefUsername = _Sensitive # pUsername_
    , _auaefEventId = pEventId_
    , _auaefFeedbackValue = pFeedbackValue_
    }


-- | The user pool ID.
auaefUserPoolId :: Lens' AdminUpdateAuthEventFeedback Text
auaefUserPoolId = lens _auaefUserPoolId (\ s a -> s{_auaefUserPoolId = a})

-- | The user pool username.
auaefUsername :: Lens' AdminUpdateAuthEventFeedback Text
auaefUsername = lens _auaefUsername (\ s a -> s{_auaefUsername = a}) . _Sensitive

-- | The authentication event ID.
auaefEventId :: Lens' AdminUpdateAuthEventFeedback Text
auaefEventId = lens _auaefEventId (\ s a -> s{_auaefEventId = a})

-- | The authentication event feedback value.
auaefFeedbackValue :: Lens' AdminUpdateAuthEventFeedback FeedbackValueType
auaefFeedbackValue = lens _auaefFeedbackValue (\ s a -> s{_auaefFeedbackValue = a})

instance AWSRequest AdminUpdateAuthEventFeedback
         where
        type Rs AdminUpdateAuthEventFeedback =
             AdminUpdateAuthEventFeedbackResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminUpdateAuthEventFeedbackResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminUpdateAuthEventFeedback where

instance NFData AdminUpdateAuthEventFeedback where

instance ToHeaders AdminUpdateAuthEventFeedback where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminUpdateAuthEventFeedback"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminUpdateAuthEventFeedback where
        toJSON AdminUpdateAuthEventFeedback'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _auaefUserPoolId),
                  Just ("Username" .= _auaefUsername),
                  Just ("EventId" .= _auaefEventId),
                  Just ("FeedbackValue" .= _auaefFeedbackValue)])

instance ToPath AdminUpdateAuthEventFeedback where
        toPath = const "/"

instance ToQuery AdminUpdateAuthEventFeedback where
        toQuery = const mempty

-- | /See:/ 'adminUpdateAuthEventFeedbackResponse' smart constructor.
newtype AdminUpdateAuthEventFeedbackResponse = AdminUpdateAuthEventFeedbackResponse'
  { _auaefrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUpdateAuthEventFeedbackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auaefrsResponseStatus' - -- | The response status code.
adminUpdateAuthEventFeedbackResponse
    :: Int -- ^ 'auaefrsResponseStatus'
    -> AdminUpdateAuthEventFeedbackResponse
adminUpdateAuthEventFeedbackResponse pResponseStatus_ =
  AdminUpdateAuthEventFeedbackResponse'
    {_auaefrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
auaefrsResponseStatus :: Lens' AdminUpdateAuthEventFeedbackResponse Int
auaefrsResponseStatus = lens _auaefrsResponseStatus (\ s a -> s{_auaefrsResponseStatus = a})

instance NFData AdminUpdateAuthEventFeedbackResponse
         where
