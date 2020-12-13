{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides feedback for an authentication event as to whether it was from a valid user. This feedback is used for improving the risk evaluation decision for the user pool as part of Amazon Cognito advanced security.
module Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
  ( -- * Creating a request
    AdminUpdateAuthEventFeedback (..),
    mkAdminUpdateAuthEventFeedback,

    -- ** Request lenses
    auaefFeedbackValue,
    auaefUserPoolId,
    auaefUsername,
    auaefEventId,

    -- * Destructuring the response
    AdminUpdateAuthEventFeedbackResponse (..),
    mkAdminUpdateAuthEventFeedbackResponse,

    -- ** Response lenses
    auaefrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminUpdateAuthEventFeedback' smart constructor.
data AdminUpdateAuthEventFeedback = AdminUpdateAuthEventFeedback'
  { -- | The authentication event feedback value.
    feedbackValue :: FeedbackValueType,
    -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The user pool username.
    username :: Lude.Sensitive Lude.Text,
    -- | The authentication event ID.
    eventId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminUpdateAuthEventFeedback' with the minimum fields required to make a request.
--
-- * 'feedbackValue' - The authentication event feedback value.
-- * 'userPoolId' - The user pool ID.
-- * 'username' - The user pool username.
-- * 'eventId' - The authentication event ID.
mkAdminUpdateAuthEventFeedback ::
  -- | 'feedbackValue'
  FeedbackValueType ->
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'eventId'
  Lude.Text ->
  AdminUpdateAuthEventFeedback
mkAdminUpdateAuthEventFeedback
  pFeedbackValue_
  pUserPoolId_
  pUsername_
  pEventId_ =
    AdminUpdateAuthEventFeedback'
      { feedbackValue = pFeedbackValue_,
        userPoolId = pUserPoolId_,
        username = pUsername_,
        eventId = pEventId_
      }

-- | The authentication event feedback value.
--
-- /Note:/ Consider using 'feedbackValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefFeedbackValue :: Lens.Lens' AdminUpdateAuthEventFeedback FeedbackValueType
auaefFeedbackValue = Lens.lens (feedbackValue :: AdminUpdateAuthEventFeedback -> FeedbackValueType) (\s a -> s {feedbackValue = a} :: AdminUpdateAuthEventFeedback)
{-# DEPRECATED auaefFeedbackValue "Use generic-lens or generic-optics with 'feedbackValue' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefUserPoolId :: Lens.Lens' AdminUpdateAuthEventFeedback Lude.Text
auaefUserPoolId = Lens.lens (userPoolId :: AdminUpdateAuthEventFeedback -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminUpdateAuthEventFeedback)
{-# DEPRECATED auaefUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user pool username.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefUsername :: Lens.Lens' AdminUpdateAuthEventFeedback (Lude.Sensitive Lude.Text)
auaefUsername = Lens.lens (username :: AdminUpdateAuthEventFeedback -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminUpdateAuthEventFeedback)
{-# DEPRECATED auaefUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The authentication event ID.
--
-- /Note:/ Consider using 'eventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefEventId :: Lens.Lens' AdminUpdateAuthEventFeedback Lude.Text
auaefEventId = Lens.lens (eventId :: AdminUpdateAuthEventFeedback -> Lude.Text) (\s a -> s {eventId = a} :: AdminUpdateAuthEventFeedback)
{-# DEPRECATED auaefEventId "Use generic-lens or generic-optics with 'eventId' instead." #-}

instance Lude.AWSRequest AdminUpdateAuthEventFeedback where
  type
    Rs AdminUpdateAuthEventFeedback =
      AdminUpdateAuthEventFeedbackResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminUpdateAuthEventFeedbackResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminUpdateAuthEventFeedback where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminUpdateAuthEventFeedback" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminUpdateAuthEventFeedback where
  toJSON AdminUpdateAuthEventFeedback' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FeedbackValue" Lude..= feedbackValue),
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("EventId" Lude..= eventId)
          ]
      )

instance Lude.ToPath AdminUpdateAuthEventFeedback where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminUpdateAuthEventFeedback where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminUpdateAuthEventFeedbackResponse' smart constructor.
newtype AdminUpdateAuthEventFeedbackResponse = AdminUpdateAuthEventFeedbackResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminUpdateAuthEventFeedbackResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminUpdateAuthEventFeedbackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminUpdateAuthEventFeedbackResponse
mkAdminUpdateAuthEventFeedbackResponse pResponseStatus_ =
  AdminUpdateAuthEventFeedbackResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auaefrsResponseStatus :: Lens.Lens' AdminUpdateAuthEventFeedbackResponse Lude.Int
auaefrsResponseStatus = Lens.lens (responseStatus :: AdminUpdateAuthEventFeedbackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminUpdateAuthEventFeedbackResponse)
{-# DEPRECATED auaefrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
