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
-- Module      : Amazonka.SES.GetIdentityNotificationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.SES.GetIdentityNotificationAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

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
    identities :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.mempty
    }

-- | A list of one or more identities. You can specify an identity by using
-- its name or by using its Amazon Resource Name (ARN). Examples:
-- @user\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
getIdentityNotificationAttributes_identities :: Lens.Lens' GetIdentityNotificationAttributes [Prelude.Text]
getIdentityNotificationAttributes_identities = Lens.lens (\GetIdentityNotificationAttributes' {identities} -> identities) (\s@GetIdentityNotificationAttributes' {} a -> s {identities = a} :: GetIdentityNotificationAttributes) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetIdentityNotificationAttributes
  where
  type
    AWSResponse GetIdentityNotificationAttributes =
      GetIdentityNotificationAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetIdentityNotificationAttributesResult"
      ( \s h x ->
          GetIdentityNotificationAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Data..@? "NotificationAttributes"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Data.parseXMLMap "entry" "key" "value"
                          )
      )

instance
  Prelude.Hashable
    GetIdentityNotificationAttributes
  where
  hashWithSalt
    _salt
    GetIdentityNotificationAttributes' {..} =
      _salt `Prelude.hashWithSalt` identities

instance
  Prelude.NFData
    GetIdentityNotificationAttributes
  where
  rnf GetIdentityNotificationAttributes' {..} =
    Prelude.rnf identities

instance
  Data.ToHeaders
    GetIdentityNotificationAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetIdentityNotificationAttributes
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetIdentityNotificationAttributes
  where
  toQuery GetIdentityNotificationAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetIdentityNotificationAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Identities"
          Data.=: Data.toQueryList "member" identities
      ]

-- | Represents the notification attributes for a list of identities.
--
-- /See:/ 'newGetIdentityNotificationAttributesResponse' smart constructor.
data GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A map of Identity to IdentityNotificationAttributes.
    notificationAttributes :: Prelude.HashMap Prelude.Text IdentityNotificationAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetIdentityNotificationAttributesResponse
newGetIdentityNotificationAttributesResponse
  pHttpStatus_ =
    GetIdentityNotificationAttributesResponse'
      { httpStatus =
          pHttpStatus_,
        notificationAttributes =
          Prelude.mempty
      }

-- | The response's http status code.
getIdentityNotificationAttributesResponse_httpStatus :: Lens.Lens' GetIdentityNotificationAttributesResponse Prelude.Int
getIdentityNotificationAttributesResponse_httpStatus = Lens.lens (\GetIdentityNotificationAttributesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityNotificationAttributesResponse' {} a -> s {httpStatus = a} :: GetIdentityNotificationAttributesResponse)

-- | A map of Identity to IdentityNotificationAttributes.
getIdentityNotificationAttributesResponse_notificationAttributes :: Lens.Lens' GetIdentityNotificationAttributesResponse (Prelude.HashMap Prelude.Text IdentityNotificationAttributes)
getIdentityNotificationAttributesResponse_notificationAttributes = Lens.lens (\GetIdentityNotificationAttributesResponse' {notificationAttributes} -> notificationAttributes) (\s@GetIdentityNotificationAttributesResponse' {} a -> s {notificationAttributes = a} :: GetIdentityNotificationAttributesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetIdentityNotificationAttributesResponse
  where
  rnf GetIdentityNotificationAttributesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf notificationAttributes
