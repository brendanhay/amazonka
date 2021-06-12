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
-- Module      : Network.AWS.SES.GetIdentityNotificationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a list of verified identities (email addresses and\/or domains),
-- returns a structure describing identity notification attributes.
--
-- This operation is throttled at one request per second and can only get
-- notification attributes for up to 100 identities at a time.
--
-- For more information about using notifications with Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
module Network.AWS.SES.GetIdentityNotificationAttributes
  ( -- * Creating a Request
    GetIdentityNotificationAttributes (..),
    newGetIdentityNotificationAttributes,

    -- * Request Lenses
    getIdentityNotificationAttributes_identities,

    -- * Destructuring the Response
    GetIdentityNotificationAttributesResponse (..),
    newGetIdentityNotificationAttributesResponse,

    -- * Response Lenses
    getIdentityNotificationAttributesResponse_httpStatus,
    getIdentityNotificationAttributesResponse_notificationAttributes,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to return the notification attributes for a list of
-- identities you verified with Amazon SES. For information about Amazon
-- SES notifications, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
--
-- /See:/ 'newGetIdentityNotificationAttributes' smart constructor.
data GetIdentityNotificationAttributes = GetIdentityNotificationAttributes'
  { -- | A list of one or more identities. You can specify an identity by using
    -- its name or by using its Amazon Resource Name (ARN). Examples:
    -- @user\@example.com@, @example.com@,
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    identities :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identities', 'getIdentityNotificationAttributes_identities' - A list of one or more identities. You can specify an identity by using
-- its name or by using its Amazon Resource Name (ARN). Examples:
-- @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
newGetIdentityNotificationAttributes ::
  GetIdentityNotificationAttributes
newGetIdentityNotificationAttributes =
  GetIdentityNotificationAttributes'
    { identities =
        Core.mempty
    }

-- | A list of one or more identities. You can specify an identity by using
-- its name or by using its Amazon Resource Name (ARN). Examples:
-- @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
getIdentityNotificationAttributes_identities :: Lens.Lens' GetIdentityNotificationAttributes [Core.Text]
getIdentityNotificationAttributes_identities = Lens.lens (\GetIdentityNotificationAttributes' {identities} -> identities) (\s@GetIdentityNotificationAttributes' {} a -> s {identities = a} :: GetIdentityNotificationAttributes) Core.. Lens._Coerce

instance
  Core.AWSRequest
    GetIdentityNotificationAttributes
  where
  type
    AWSResponse GetIdentityNotificationAttributes =
      GetIdentityNotificationAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetIdentityNotificationAttributesResult"
      ( \s h x ->
          GetIdentityNotificationAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "NotificationAttributes"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLMap "entry" "key" "value"
                     )
      )

instance
  Core.Hashable
    GetIdentityNotificationAttributes

instance
  Core.NFData
    GetIdentityNotificationAttributes

instance
  Core.ToHeaders
    GetIdentityNotificationAttributes
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetIdentityNotificationAttributes
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetIdentityNotificationAttributes
  where
  toQuery GetIdentityNotificationAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetIdentityNotificationAttributes" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "Identities"
          Core.=: Core.toQueryList "member" identities
      ]

-- | Represents the notification attributes for a list of identities.
--
-- /See:/ 'newGetIdentityNotificationAttributesResponse' smart constructor.
data GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A map of Identity to IdentityNotificationAttributes.
    notificationAttributes :: Core.HashMap Core.Text IdentityNotificationAttributes
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityNotificationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getIdentityNotificationAttributesResponse_httpStatus' - The response's http status code.
--
-- 'notificationAttributes', 'getIdentityNotificationAttributesResponse_notificationAttributes' - A map of Identity to IdentityNotificationAttributes.
newGetIdentityNotificationAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetIdentityNotificationAttributesResponse
newGetIdentityNotificationAttributesResponse
  pHttpStatus_ =
    GetIdentityNotificationAttributesResponse'
      { httpStatus =
          pHttpStatus_,
        notificationAttributes =
          Core.mempty
      }

-- | The response's http status code.
getIdentityNotificationAttributesResponse_httpStatus :: Lens.Lens' GetIdentityNotificationAttributesResponse Core.Int
getIdentityNotificationAttributesResponse_httpStatus = Lens.lens (\GetIdentityNotificationAttributesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityNotificationAttributesResponse' {} a -> s {httpStatus = a} :: GetIdentityNotificationAttributesResponse)

-- | A map of Identity to IdentityNotificationAttributes.
getIdentityNotificationAttributesResponse_notificationAttributes :: Lens.Lens' GetIdentityNotificationAttributesResponse (Core.HashMap Core.Text IdentityNotificationAttributes)
getIdentityNotificationAttributesResponse_notificationAttributes = Lens.lens (\GetIdentityNotificationAttributesResponse' {notificationAttributes} -> notificationAttributes) (\s@GetIdentityNotificationAttributesResponse' {} a -> s {notificationAttributes = a} :: GetIdentityNotificationAttributesResponse) Core.. Lens._Coerce

instance
  Core.NFData
    GetIdentityNotificationAttributesResponse
