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
-- Module      : Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given an identity (an email address or a domain), sets whether Amazon
-- SES includes the original email headers in the Amazon Simple
-- Notification Service (Amazon SNS) notifications of a specified type.
--
-- You can execute this operation no more than once per second.
--
-- For more information about using notifications with Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
module Network.AWS.SES.SetIdentityHeadersInNotificationsEnabled
  ( -- * Creating a Request
    SetIdentityHeadersInNotificationsEnabled (..),
    newSetIdentityHeadersInNotificationsEnabled,

    -- * Request Lenses
    setIdentityHeadersInNotificationsEnabled_identity,
    setIdentityHeadersInNotificationsEnabled_notificationType,
    setIdentityHeadersInNotificationsEnabled_enabled,

    -- * Destructuring the Response
    SetIdentityHeadersInNotificationsEnabledResponse (..),
    newSetIdentityHeadersInNotificationsEnabledResponse,

    -- * Response Lenses
    setIdentityHeadersInNotificationsEnabledResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to set whether Amazon SES includes the original
-- email headers in the Amazon SNS notifications of a specified type. For
-- information about notifications, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetIdentityHeadersInNotificationsEnabled' smart constructor.
data SetIdentityHeadersInNotificationsEnabled = SetIdentityHeadersInNotificationsEnabled'
  { -- | The identity for which to enable or disable headers in notifications.
    -- Examples: @user\@example.com@, @example.com@.
    identity :: Prelude.Text,
    -- | The notification type for which to enable or disable headers in
    -- notifications.
    notificationType :: NotificationType,
    -- | Sets whether Amazon SES includes the original email headers in Amazon
    -- SNS notifications of the specified notification type. A value of @true@
    -- specifies that Amazon SES will include headers in notifications, and a
    -- value of @false@ specifies that Amazon SES will not include headers in
    -- notifications.
    --
    -- This value can only be set when @NotificationType@ is already set to use
    -- a particular Amazon SNS topic.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityHeadersInNotificationsEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'setIdentityHeadersInNotificationsEnabled_identity' - The identity for which to enable or disable headers in notifications.
-- Examples: @user\@example.com@, @example.com@.
--
-- 'notificationType', 'setIdentityHeadersInNotificationsEnabled_notificationType' - The notification type for which to enable or disable headers in
-- notifications.
--
-- 'enabled', 'setIdentityHeadersInNotificationsEnabled_enabled' - Sets whether Amazon SES includes the original email headers in Amazon
-- SNS notifications of the specified notification type. A value of @true@
-- specifies that Amazon SES will include headers in notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- notifications.
--
-- This value can only be set when @NotificationType@ is already set to use
-- a particular Amazon SNS topic.
newSetIdentityHeadersInNotificationsEnabled ::
  -- | 'identity'
  Prelude.Text ->
  -- | 'notificationType'
  NotificationType ->
  -- | 'enabled'
  Prelude.Bool ->
  SetIdentityHeadersInNotificationsEnabled
newSetIdentityHeadersInNotificationsEnabled
  pIdentity_
  pNotificationType_
  pEnabled_ =
    SetIdentityHeadersInNotificationsEnabled'
      { identity =
          pIdentity_,
        notificationType =
          pNotificationType_,
        enabled = pEnabled_
      }

-- | The identity for which to enable or disable headers in notifications.
-- Examples: @user\@example.com@, @example.com@.
setIdentityHeadersInNotificationsEnabled_identity :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled Prelude.Text
setIdentityHeadersInNotificationsEnabled_identity = Lens.lens (\SetIdentityHeadersInNotificationsEnabled' {identity} -> identity) (\s@SetIdentityHeadersInNotificationsEnabled' {} a -> s {identity = a} :: SetIdentityHeadersInNotificationsEnabled)

-- | The notification type for which to enable or disable headers in
-- notifications.
setIdentityHeadersInNotificationsEnabled_notificationType :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled NotificationType
setIdentityHeadersInNotificationsEnabled_notificationType = Lens.lens (\SetIdentityHeadersInNotificationsEnabled' {notificationType} -> notificationType) (\s@SetIdentityHeadersInNotificationsEnabled' {} a -> s {notificationType = a} :: SetIdentityHeadersInNotificationsEnabled)

-- | Sets whether Amazon SES includes the original email headers in Amazon
-- SNS notifications of the specified notification type. A value of @true@
-- specifies that Amazon SES will include headers in notifications, and a
-- value of @false@ specifies that Amazon SES will not include headers in
-- notifications.
--
-- This value can only be set when @NotificationType@ is already set to use
-- a particular Amazon SNS topic.
setIdentityHeadersInNotificationsEnabled_enabled :: Lens.Lens' SetIdentityHeadersInNotificationsEnabled Prelude.Bool
setIdentityHeadersInNotificationsEnabled_enabled = Lens.lens (\SetIdentityHeadersInNotificationsEnabled' {enabled} -> enabled) (\s@SetIdentityHeadersInNotificationsEnabled' {} a -> s {enabled = a} :: SetIdentityHeadersInNotificationsEnabled)

instance
  Prelude.AWSRequest
    SetIdentityHeadersInNotificationsEnabled
  where
  type
    Rs SetIdentityHeadersInNotificationsEnabled =
      SetIdentityHeadersInNotificationsEnabledResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetIdentityHeadersInNotificationsEnabledResult"
      ( \s h x ->
          SetIdentityHeadersInNotificationsEnabledResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetIdentityHeadersInNotificationsEnabled

instance
  Prelude.NFData
    SetIdentityHeadersInNotificationsEnabled

instance
  Prelude.ToHeaders
    SetIdentityHeadersInNotificationsEnabled
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    SetIdentityHeadersInNotificationsEnabled
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    SetIdentityHeadersInNotificationsEnabled
  where
  toQuery SetIdentityHeadersInNotificationsEnabled' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "SetIdentityHeadersInNotificationsEnabled" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Prelude.=: identity,
        "NotificationType" Prelude.=: notificationType,
        "Enabled" Prelude.=: enabled
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetIdentityHeadersInNotificationsEnabledResponse' smart constructor.
data SetIdentityHeadersInNotificationsEnabledResponse = SetIdentityHeadersInNotificationsEnabledResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityHeadersInNotificationsEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setIdentityHeadersInNotificationsEnabledResponse_httpStatus' - The response's http status code.
newSetIdentityHeadersInNotificationsEnabledResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetIdentityHeadersInNotificationsEnabledResponse
newSetIdentityHeadersInNotificationsEnabledResponse
  pHttpStatus_ =
    SetIdentityHeadersInNotificationsEnabledResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
setIdentityHeadersInNotificationsEnabledResponse_httpStatus :: Lens.Lens' SetIdentityHeadersInNotificationsEnabledResponse Prelude.Int
setIdentityHeadersInNotificationsEnabledResponse_httpStatus = Lens.lens (\SetIdentityHeadersInNotificationsEnabledResponse' {httpStatus} -> httpStatus) (\s@SetIdentityHeadersInNotificationsEnabledResponse' {} a -> s {httpStatus = a} :: SetIdentityHeadersInNotificationsEnabledResponse)

instance
  Prelude.NFData
    SetIdentityHeadersInNotificationsEnabledResponse
