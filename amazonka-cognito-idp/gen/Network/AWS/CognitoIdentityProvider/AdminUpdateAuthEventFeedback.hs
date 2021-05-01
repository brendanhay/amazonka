{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides feedback for an authentication event as to whether it was from
-- a valid user. This feedback is used for improving the risk evaluation
-- decision for the user pool as part of Amazon Cognito advanced security.
module Network.AWS.CognitoIdentityProvider.AdminUpdateAuthEventFeedback
  ( -- * Creating a Request
    AdminUpdateAuthEventFeedback (..),
    newAdminUpdateAuthEventFeedback,

    -- * Request Lenses
    adminUpdateAuthEventFeedback_userPoolId,
    adminUpdateAuthEventFeedback_username,
    adminUpdateAuthEventFeedback_eventId,
    adminUpdateAuthEventFeedback_feedbackValue,

    -- * Destructuring the Response
    AdminUpdateAuthEventFeedbackResponse (..),
    newAdminUpdateAuthEventFeedbackResponse,

    -- * Response Lenses
    adminUpdateAuthEventFeedbackResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAdminUpdateAuthEventFeedback' smart constructor.
data AdminUpdateAuthEventFeedback = AdminUpdateAuthEventFeedback'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user pool username.
    username :: Prelude.Sensitive Prelude.Text,
    -- | The authentication event ID.
    eventId :: Prelude.Text,
    -- | The authentication event feedback value.
    feedbackValue :: FeedbackValueType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminUpdateAuthEventFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'adminUpdateAuthEventFeedback_userPoolId' - The user pool ID.
--
-- 'username', 'adminUpdateAuthEventFeedback_username' - The user pool username.
--
-- 'eventId', 'adminUpdateAuthEventFeedback_eventId' - The authentication event ID.
--
-- 'feedbackValue', 'adminUpdateAuthEventFeedback_feedbackValue' - The authentication event feedback value.
newAdminUpdateAuthEventFeedback ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'eventId'
  Prelude.Text ->
  -- | 'feedbackValue'
  FeedbackValueType ->
  AdminUpdateAuthEventFeedback
newAdminUpdateAuthEventFeedback
  pUserPoolId_
  pUsername_
  pEventId_
  pFeedbackValue_ =
    AdminUpdateAuthEventFeedback'
      { userPoolId =
          pUserPoolId_,
        username =
          Prelude._Sensitive Lens.# pUsername_,
        eventId = pEventId_,
        feedbackValue = pFeedbackValue_
      }

-- | The user pool ID.
adminUpdateAuthEventFeedback_userPoolId :: Lens.Lens' AdminUpdateAuthEventFeedback Prelude.Text
adminUpdateAuthEventFeedback_userPoolId = Lens.lens (\AdminUpdateAuthEventFeedback' {userPoolId} -> userPoolId) (\s@AdminUpdateAuthEventFeedback' {} a -> s {userPoolId = a} :: AdminUpdateAuthEventFeedback)

-- | The user pool username.
adminUpdateAuthEventFeedback_username :: Lens.Lens' AdminUpdateAuthEventFeedback Prelude.Text
adminUpdateAuthEventFeedback_username = Lens.lens (\AdminUpdateAuthEventFeedback' {username} -> username) (\s@AdminUpdateAuthEventFeedback' {} a -> s {username = a} :: AdminUpdateAuthEventFeedback) Prelude.. Prelude._Sensitive

-- | The authentication event ID.
adminUpdateAuthEventFeedback_eventId :: Lens.Lens' AdminUpdateAuthEventFeedback Prelude.Text
adminUpdateAuthEventFeedback_eventId = Lens.lens (\AdminUpdateAuthEventFeedback' {eventId} -> eventId) (\s@AdminUpdateAuthEventFeedback' {} a -> s {eventId = a} :: AdminUpdateAuthEventFeedback)

-- | The authentication event feedback value.
adminUpdateAuthEventFeedback_feedbackValue :: Lens.Lens' AdminUpdateAuthEventFeedback FeedbackValueType
adminUpdateAuthEventFeedback_feedbackValue = Lens.lens (\AdminUpdateAuthEventFeedback' {feedbackValue} -> feedbackValue) (\s@AdminUpdateAuthEventFeedback' {} a -> s {feedbackValue = a} :: AdminUpdateAuthEventFeedback)

instance
  Prelude.AWSRequest
    AdminUpdateAuthEventFeedback
  where
  type
    Rs AdminUpdateAuthEventFeedback =
      AdminUpdateAuthEventFeedbackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AdminUpdateAuthEventFeedbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AdminUpdateAuthEventFeedback

instance Prelude.NFData AdminUpdateAuthEventFeedback

instance
  Prelude.ToHeaders
    AdminUpdateAuthEventFeedback
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.AdminUpdateAuthEventFeedback" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AdminUpdateAuthEventFeedback where
  toJSON AdminUpdateAuthEventFeedback' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just ("Username" Prelude..= username),
            Prelude.Just ("EventId" Prelude..= eventId),
            Prelude.Just
              ("FeedbackValue" Prelude..= feedbackValue)
          ]
      )

instance Prelude.ToPath AdminUpdateAuthEventFeedback where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AdminUpdateAuthEventFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAdminUpdateAuthEventFeedbackResponse' smart constructor.
data AdminUpdateAuthEventFeedbackResponse = AdminUpdateAuthEventFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdminUpdateAuthEventFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'adminUpdateAuthEventFeedbackResponse_httpStatus' - The response's http status code.
newAdminUpdateAuthEventFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AdminUpdateAuthEventFeedbackResponse
newAdminUpdateAuthEventFeedbackResponse pHttpStatus_ =
  AdminUpdateAuthEventFeedbackResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
adminUpdateAuthEventFeedbackResponse_httpStatus :: Lens.Lens' AdminUpdateAuthEventFeedbackResponse Prelude.Int
adminUpdateAuthEventFeedbackResponse_httpStatus = Lens.lens (\AdminUpdateAuthEventFeedbackResponse' {httpStatus} -> httpStatus) (\s@AdminUpdateAuthEventFeedbackResponse' {} a -> s {httpStatus = a} :: AdminUpdateAuthEventFeedbackResponse)

instance
  Prelude.NFData
    AdminUpdateAuthEventFeedbackResponse
