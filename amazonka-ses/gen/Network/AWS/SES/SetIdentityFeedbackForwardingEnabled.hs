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
-- Module      : Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (an email address or a domain), enables or disables
-- whether Amazon SES forwards bounce and complaint notifications as email.
-- Feedback forwarding can only be disabled when Amazon Simple Notification
-- Service (Amazon SNS) topics are specified for both bounces and
-- complaints.
--
-- Feedback forwarding does not apply to delivery notifications. Delivery
-- notifications are only available through Amazon SNS.
--
-- You can execute this operation no more than once per second.
--
-- For more information about using notifications with Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
module Network.AWS.SES.SetIdentityFeedbackForwardingEnabled
  ( -- * Creating a Request
    SetIdentityFeedbackForwardingEnabled (..),
    newSetIdentityFeedbackForwardingEnabled,

    -- * Request Lenses
    setIdentityFeedbackForwardingEnabled_identity,
    setIdentityFeedbackForwardingEnabled_forwardingEnabled,

    -- * Destructuring the Response
    SetIdentityFeedbackForwardingEnabledResponse (..),
    newSetIdentityFeedbackForwardingEnabledResponse,

    -- * Response Lenses
    setIdentityFeedbackForwardingEnabledResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to enable or disable whether Amazon SES forwards
-- you bounce and complaint notifications through email. For information
-- about email feedback forwarding, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-email.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetIdentityFeedbackForwardingEnabled' smart constructor.
data SetIdentityFeedbackForwardingEnabled = SetIdentityFeedbackForwardingEnabled'
  { -- | The identity for which to set bounce and complaint notification
    -- forwarding. Examples: @user\@example.com@, @example.com@.
    identity :: Prelude.Text,
    -- | Sets whether Amazon SES will forward bounce and complaint notifications
    -- as email. @true@ specifies that Amazon SES will forward bounce and
    -- complaint notifications as email, in addition to any Amazon SNS topic
    -- publishing otherwise specified. @false@ specifies that Amazon SES will
    -- publish bounce and complaint notifications only through Amazon SNS. This
    -- value can only be set to @false@ when Amazon SNS topics are set for both
    -- @Bounce@ and @Complaint@ notification types.
    forwardingEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityFeedbackForwardingEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'setIdentityFeedbackForwardingEnabled_identity' - The identity for which to set bounce and complaint notification
-- forwarding. Examples: @user\@example.com@, @example.com@.
--
-- 'forwardingEnabled', 'setIdentityFeedbackForwardingEnabled_forwardingEnabled' - Sets whether Amazon SES will forward bounce and complaint notifications
-- as email. @true@ specifies that Amazon SES will forward bounce and
-- complaint notifications as email, in addition to any Amazon SNS topic
-- publishing otherwise specified. @false@ specifies that Amazon SES will
-- publish bounce and complaint notifications only through Amazon SNS. This
-- value can only be set to @false@ when Amazon SNS topics are set for both
-- @Bounce@ and @Complaint@ notification types.
newSetIdentityFeedbackForwardingEnabled ::
  -- | 'identity'
  Prelude.Text ->
  -- | 'forwardingEnabled'
  Prelude.Bool ->
  SetIdentityFeedbackForwardingEnabled
newSetIdentityFeedbackForwardingEnabled
  pIdentity_
  pForwardingEnabled_ =
    SetIdentityFeedbackForwardingEnabled'
      { identity =
          pIdentity_,
        forwardingEnabled =
          pForwardingEnabled_
      }

-- | The identity for which to set bounce and complaint notification
-- forwarding. Examples: @user\@example.com@, @example.com@.
setIdentityFeedbackForwardingEnabled_identity :: Lens.Lens' SetIdentityFeedbackForwardingEnabled Prelude.Text
setIdentityFeedbackForwardingEnabled_identity = Lens.lens (\SetIdentityFeedbackForwardingEnabled' {identity} -> identity) (\s@SetIdentityFeedbackForwardingEnabled' {} a -> s {identity = a} :: SetIdentityFeedbackForwardingEnabled)

-- | Sets whether Amazon SES will forward bounce and complaint notifications
-- as email. @true@ specifies that Amazon SES will forward bounce and
-- complaint notifications as email, in addition to any Amazon SNS topic
-- publishing otherwise specified. @false@ specifies that Amazon SES will
-- publish bounce and complaint notifications only through Amazon SNS. This
-- value can only be set to @false@ when Amazon SNS topics are set for both
-- @Bounce@ and @Complaint@ notification types.
setIdentityFeedbackForwardingEnabled_forwardingEnabled :: Lens.Lens' SetIdentityFeedbackForwardingEnabled Prelude.Bool
setIdentityFeedbackForwardingEnabled_forwardingEnabled = Lens.lens (\SetIdentityFeedbackForwardingEnabled' {forwardingEnabled} -> forwardingEnabled) (\s@SetIdentityFeedbackForwardingEnabled' {} a -> s {forwardingEnabled = a} :: SetIdentityFeedbackForwardingEnabled)

instance
  Prelude.AWSRequest
    SetIdentityFeedbackForwardingEnabled
  where
  type
    Rs SetIdentityFeedbackForwardingEnabled =
      SetIdentityFeedbackForwardingEnabledResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetIdentityFeedbackForwardingEnabledResult"
      ( \s h x ->
          SetIdentityFeedbackForwardingEnabledResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetIdentityFeedbackForwardingEnabled

instance
  Prelude.NFData
    SetIdentityFeedbackForwardingEnabled

instance
  Prelude.ToHeaders
    SetIdentityFeedbackForwardingEnabled
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    SetIdentityFeedbackForwardingEnabled
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    SetIdentityFeedbackForwardingEnabled
  where
  toQuery SetIdentityFeedbackForwardingEnabled' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "SetIdentityFeedbackForwardingEnabled" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Prelude.=: identity,
        "ForwardingEnabled" Prelude.=: forwardingEnabled
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetIdentityFeedbackForwardingEnabledResponse' smart constructor.
data SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityFeedbackForwardingEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setIdentityFeedbackForwardingEnabledResponse_httpStatus' - The response's http status code.
newSetIdentityFeedbackForwardingEnabledResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetIdentityFeedbackForwardingEnabledResponse
newSetIdentityFeedbackForwardingEnabledResponse
  pHttpStatus_ =
    SetIdentityFeedbackForwardingEnabledResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
setIdentityFeedbackForwardingEnabledResponse_httpStatus :: Lens.Lens' SetIdentityFeedbackForwardingEnabledResponse Prelude.Int
setIdentityFeedbackForwardingEnabledResponse_httpStatus = Lens.lens (\SetIdentityFeedbackForwardingEnabledResponse' {httpStatus} -> httpStatus) (\s@SetIdentityFeedbackForwardingEnabledResponse' {} a -> s {httpStatus = a} :: SetIdentityFeedbackForwardingEnabledResponse)

instance
  Prelude.NFData
    SetIdentityFeedbackForwardingEnabledResponse
