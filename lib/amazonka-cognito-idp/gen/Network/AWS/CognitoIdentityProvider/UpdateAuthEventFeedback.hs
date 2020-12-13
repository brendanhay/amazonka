{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the feedback for an authentication event whether it was from a valid user or not. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.
module Network.AWS.CognitoIdentityProvider.UpdateAuthEventFeedback
  ( -- * Creating a request
    UpdateAuthEventFeedback (..),
    mkUpdateAuthEventFeedback,

    -- ** Request lenses
    uaefFeedbackValue,
    uaefFeedbackToken,
    uaefUserPoolId,
    uaefUsername,
    uaefEventId,

    -- * Destructuring the response
    UpdateAuthEventFeedbackResponse (..),
    mkUpdateAuthEventFeedbackResponse,

    -- ** Response lenses
    uaefrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateAuthEventFeedback' smart constructor.
data UpdateAuthEventFeedback = UpdateAuthEventFeedback'
  { -- | The authentication event feedback value.
    feedbackValue :: FeedbackValueType,
    -- | The feedback token.
    feedbackToken :: Lude.Sensitive Lude.Text,
    -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The user pool username.
    username :: Lude.Sensitive Lude.Text,
    -- | The event ID.
    eventId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAuthEventFeedback' with the minimum fields required to make a request.
--
-- * 'feedbackValue' - The authentication event feedback value.
-- * 'feedbackToken' - The feedback token.
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user pool username.
-- * 'eventId' - The event ID.
mkUpdateAuthEventFeedback ::
  -- | 'feedbackValue'
  FeedbackValueType ->
  -- | 'feedbackToken'
  Lude.Sensitive Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'eventId'
  Lude.Text ->
  UpdateAuthEventFeedback
mkUpdateAuthEventFeedback
  pFeedbackValue_
  pFeedbackToken_
  pUserPoolId_
  pUsername_
  pEventId_ =
    UpdateAuthEventFeedback'
      { feedbackValue = pFeedbackValue_,
        feedbackToken = pFeedbackToken_,
        userPoolId = pUserPoolId_,
        username = pUsername_,
        eventId = pEventId_
      }

-- | The authentication event feedback value.
--
-- /Note:/ Consider using 'feedbackValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaefFeedbackValue :: Lens.Lens' UpdateAuthEventFeedback FeedbackValueType
uaefFeedbackValue = Lens.lens (feedbackValue :: UpdateAuthEventFeedback -> FeedbackValueType) (\s a -> s {feedbackValue = a} :: UpdateAuthEventFeedback)
{-# DEPRECATED uaefFeedbackValue "Use generic-lens or generic-optics with 'feedbackValue' instead." #-}

-- | The feedback token.
--
-- /Note:/ Consider using 'feedbackToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaefFeedbackToken :: Lens.Lens' UpdateAuthEventFeedback (Lude.Sensitive Lude.Text)
uaefFeedbackToken = Lens.lens (feedbackToken :: UpdateAuthEventFeedback -> Lude.Sensitive Lude.Text) (\s a -> s {feedbackToken = a} :: UpdateAuthEventFeedback)
{-# DEPRECATED uaefFeedbackToken "Use generic-lens or generic-optics with 'feedbackToken' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaefUserPoolId :: Lens.Lens' UpdateAuthEventFeedback Lude.Text
uaefUserPoolId = Lens.lens (userPoolId :: UpdateAuthEventFeedback -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateAuthEventFeedback)
{-# DEPRECATED uaefUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user pool username.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaefUsername :: Lens.Lens' UpdateAuthEventFeedback (Lude.Sensitive Lude.Text)
uaefUsername = Lens.lens (username :: UpdateAuthEventFeedback -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: UpdateAuthEventFeedback)
{-# DEPRECATED uaefUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The event ID.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaefEventId :: Lens.Lens' UpdateAuthEventFeedback Lude.Text
uaefEventId = Lens.lens (eventId :: UpdateAuthEventFeedback -> Lude.Text) (\s a -> s {eventId = a} :: UpdateAuthEventFeedback)
{-# DEPRECATED uaefEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.AWSRequest UpdateAuthEventFeedback where
  type Rs UpdateAuthEventFeedback = UpdateAuthEventFeedbackResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateAuthEventFeedbackResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAuthEventFeedback where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateAuthEventFeedback" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAuthEventFeedback where
  toJSON UpdateAuthEventFeedback' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FeedbackValue" Lude..= feedbackValue),
            Lude.Just ("FeedbackToken" Lude..= feedbackToken),
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("EventId" Lude..= eventId)
          ]
      )

instance Lude.ToPath UpdateAuthEventFeedback where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAuthEventFeedback where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAuthEventFeedbackResponse' smart constructor.
newtype UpdateAuthEventFeedbackResponse = UpdateAuthEventFeedbackResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAuthEventFeedbackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateAuthEventFeedbackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAuthEventFeedbackResponse
mkUpdateAuthEventFeedbackResponse pResponseStatus_ =
  UpdateAuthEventFeedbackResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaefrsResponseStatus :: Lens.Lens' UpdateAuthEventFeedbackResponse Lude.Int
uaefrsResponseStatus = Lens.lens (responseStatus :: UpdateAuthEventFeedbackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAuthEventFeedbackResponse)
{-# DEPRECATED uaefrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
