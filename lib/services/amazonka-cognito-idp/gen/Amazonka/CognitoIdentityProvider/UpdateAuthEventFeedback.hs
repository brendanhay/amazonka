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
-- Module      : Amazonka.CognitoIdentityProvider.UpdateAuthEventFeedback
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the feedback for an authentication event, whether it was from a
-- valid user or not. This feedback is used for improving the risk
-- evaluation decision for the user pool as part of Amazon Cognito advanced
-- security.
module Amazonka.CognitoIdentityProvider.UpdateAuthEventFeedback
  ( -- * Creating a Request
    UpdateAuthEventFeedback (..),
    newUpdateAuthEventFeedback,

    -- * Request Lenses
    updateAuthEventFeedback_userPoolId,
    updateAuthEventFeedback_username,
    updateAuthEventFeedback_eventId,
    updateAuthEventFeedback_feedbackToken,
    updateAuthEventFeedback_feedbackValue,

    -- * Destructuring the Response
    UpdateAuthEventFeedbackResponse (..),
    newUpdateAuthEventFeedbackResponse,

    -- * Response Lenses
    updateAuthEventFeedbackResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAuthEventFeedback' smart constructor.
data UpdateAuthEventFeedback = UpdateAuthEventFeedback'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The user pool username.
    username :: Data.Sensitive Prelude.Text,
    -- | The event ID.
    eventId :: Prelude.Text,
    -- | The feedback token.
    feedbackToken :: Data.Sensitive Prelude.Text,
    -- | The authentication event feedback value.
    feedbackValue :: FeedbackValueType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuthEventFeedback' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'updateAuthEventFeedback_userPoolId' - The user pool ID.
--
-- 'username', 'updateAuthEventFeedback_username' - The user pool username.
--
-- 'eventId', 'updateAuthEventFeedback_eventId' - The event ID.
--
-- 'feedbackToken', 'updateAuthEventFeedback_feedbackToken' - The feedback token.
--
-- 'feedbackValue', 'updateAuthEventFeedback_feedbackValue' - The authentication event feedback value.
newUpdateAuthEventFeedback ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  -- | 'eventId'
  Prelude.Text ->
  -- | 'feedbackToken'
  Prelude.Text ->
  -- | 'feedbackValue'
  FeedbackValueType ->
  UpdateAuthEventFeedback
newUpdateAuthEventFeedback
  pUserPoolId_
  pUsername_
  pEventId_
  pFeedbackToken_
  pFeedbackValue_ =
    UpdateAuthEventFeedback'
      { userPoolId = pUserPoolId_,
        username = Data._Sensitive Lens.# pUsername_,
        eventId = pEventId_,
        feedbackToken =
          Data._Sensitive Lens.# pFeedbackToken_,
        feedbackValue = pFeedbackValue_
      }

-- | The user pool ID.
updateAuthEventFeedback_userPoolId :: Lens.Lens' UpdateAuthEventFeedback Prelude.Text
updateAuthEventFeedback_userPoolId = Lens.lens (\UpdateAuthEventFeedback' {userPoolId} -> userPoolId) (\s@UpdateAuthEventFeedback' {} a -> s {userPoolId = a} :: UpdateAuthEventFeedback)

-- | The user pool username.
updateAuthEventFeedback_username :: Lens.Lens' UpdateAuthEventFeedback Prelude.Text
updateAuthEventFeedback_username = Lens.lens (\UpdateAuthEventFeedback' {username} -> username) (\s@UpdateAuthEventFeedback' {} a -> s {username = a} :: UpdateAuthEventFeedback) Prelude.. Data._Sensitive

-- | The event ID.
updateAuthEventFeedback_eventId :: Lens.Lens' UpdateAuthEventFeedback Prelude.Text
updateAuthEventFeedback_eventId = Lens.lens (\UpdateAuthEventFeedback' {eventId} -> eventId) (\s@UpdateAuthEventFeedback' {} a -> s {eventId = a} :: UpdateAuthEventFeedback)

-- | The feedback token.
updateAuthEventFeedback_feedbackToken :: Lens.Lens' UpdateAuthEventFeedback Prelude.Text
updateAuthEventFeedback_feedbackToken = Lens.lens (\UpdateAuthEventFeedback' {feedbackToken} -> feedbackToken) (\s@UpdateAuthEventFeedback' {} a -> s {feedbackToken = a} :: UpdateAuthEventFeedback) Prelude.. Data._Sensitive

-- | The authentication event feedback value.
updateAuthEventFeedback_feedbackValue :: Lens.Lens' UpdateAuthEventFeedback FeedbackValueType
updateAuthEventFeedback_feedbackValue = Lens.lens (\UpdateAuthEventFeedback' {feedbackValue} -> feedbackValue) (\s@UpdateAuthEventFeedback' {} a -> s {feedbackValue = a} :: UpdateAuthEventFeedback)

instance Core.AWSRequest UpdateAuthEventFeedback where
  type
    AWSResponse UpdateAuthEventFeedback =
      UpdateAuthEventFeedbackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAuthEventFeedbackResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAuthEventFeedback where
  hashWithSalt _salt UpdateAuthEventFeedback' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` feedbackToken
      `Prelude.hashWithSalt` feedbackValue

instance Prelude.NFData UpdateAuthEventFeedback where
  rnf UpdateAuthEventFeedback' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf feedbackToken
      `Prelude.seq` Prelude.rnf feedbackValue

instance Data.ToHeaders UpdateAuthEventFeedback where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.UpdateAuthEventFeedback" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAuthEventFeedback where
  toJSON UpdateAuthEventFeedback' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Username" Data..= username),
            Prelude.Just ("EventId" Data..= eventId),
            Prelude.Just ("FeedbackToken" Data..= feedbackToken),
            Prelude.Just
              ("FeedbackValue" Data..= feedbackValue)
          ]
      )

instance Data.ToPath UpdateAuthEventFeedback where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAuthEventFeedback where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAuthEventFeedbackResponse' smart constructor.
data UpdateAuthEventFeedbackResponse = UpdateAuthEventFeedbackResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuthEventFeedbackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAuthEventFeedbackResponse_httpStatus' - The response's http status code.
newUpdateAuthEventFeedbackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAuthEventFeedbackResponse
newUpdateAuthEventFeedbackResponse pHttpStatus_ =
  UpdateAuthEventFeedbackResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateAuthEventFeedbackResponse_httpStatus :: Lens.Lens' UpdateAuthEventFeedbackResponse Prelude.Int
updateAuthEventFeedbackResponse_httpStatus = Lens.lens (\UpdateAuthEventFeedbackResponse' {httpStatus} -> httpStatus) (\s@UpdateAuthEventFeedbackResponse' {} a -> s {httpStatus = a} :: UpdateAuthEventFeedbackResponse)

instance
  Prelude.NFData
    UpdateAuthEventFeedbackResponse
  where
  rnf UpdateAuthEventFeedbackResponse' {..} =
    Prelude.rnf httpStatus
