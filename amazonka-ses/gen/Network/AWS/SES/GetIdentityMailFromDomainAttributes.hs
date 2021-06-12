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
-- Module      : Network.AWS.SES.GetIdentityMailFromDomainAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the custom MAIL FROM attributes for a list of identities (email
-- addresses : domains).
--
-- This operation is throttled at one request per second and can only get
-- custom MAIL FROM attributes for up to 100 identities at a time.
module Network.AWS.SES.GetIdentityMailFromDomainAttributes
  ( -- * Creating a Request
    GetIdentityMailFromDomainAttributes (..),
    newGetIdentityMailFromDomainAttributes,

    -- * Request Lenses
    getIdentityMailFromDomainAttributes_identities,

    -- * Destructuring the Response
    GetIdentityMailFromDomainAttributesResponse (..),
    newGetIdentityMailFromDomainAttributesResponse,

    -- * Response Lenses
    getIdentityMailFromDomainAttributesResponse_httpStatus,
    getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to return the Amazon SES custom MAIL FROM
-- attributes for a list of identities. For information about using a
-- custom MAIL FROM domain, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide>.
--
-- /See:/ 'newGetIdentityMailFromDomainAttributes' smart constructor.
data GetIdentityMailFromDomainAttributes = GetIdentityMailFromDomainAttributes'
  { -- | A list of one or more identities.
    identities :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityMailFromDomainAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identities', 'getIdentityMailFromDomainAttributes_identities' - A list of one or more identities.
newGetIdentityMailFromDomainAttributes ::
  GetIdentityMailFromDomainAttributes
newGetIdentityMailFromDomainAttributes =
  GetIdentityMailFromDomainAttributes'
    { identities =
        Core.mempty
    }

-- | A list of one or more identities.
getIdentityMailFromDomainAttributes_identities :: Lens.Lens' GetIdentityMailFromDomainAttributes [Core.Text]
getIdentityMailFromDomainAttributes_identities = Lens.lens (\GetIdentityMailFromDomainAttributes' {identities} -> identities) (\s@GetIdentityMailFromDomainAttributes' {} a -> s {identities = a} :: GetIdentityMailFromDomainAttributes) Core.. Lens._Coerce

instance
  Core.AWSRequest
    GetIdentityMailFromDomainAttributes
  where
  type
    AWSResponse GetIdentityMailFromDomainAttributes =
      GetIdentityMailFromDomainAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetIdentityMailFromDomainAttributesResult"
      ( \s h x ->
          GetIdentityMailFromDomainAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "MailFromDomainAttributes"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLMap "entry" "key" "value"
                     )
      )

instance
  Core.Hashable
    GetIdentityMailFromDomainAttributes

instance
  Core.NFData
    GetIdentityMailFromDomainAttributes

instance
  Core.ToHeaders
    GetIdentityMailFromDomainAttributes
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetIdentityMailFromDomainAttributes
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetIdentityMailFromDomainAttributes
  where
  toQuery GetIdentityMailFromDomainAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetIdentityMailFromDomainAttributes" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "Identities"
          Core.=: Core.toQueryList "member" identities
      ]

-- | Represents the custom MAIL FROM attributes for a list of identities.
--
-- /See:/ 'newGetIdentityMailFromDomainAttributesResponse' smart constructor.
data GetIdentityMailFromDomainAttributesResponse = GetIdentityMailFromDomainAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A map of identities to custom MAIL FROM attributes.
    mailFromDomainAttributes :: Core.HashMap Core.Text IdentityMailFromDomainAttributes
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityMailFromDomainAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getIdentityMailFromDomainAttributesResponse_httpStatus' - The response's http status code.
--
-- 'mailFromDomainAttributes', 'getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes' - A map of identities to custom MAIL FROM attributes.
newGetIdentityMailFromDomainAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetIdentityMailFromDomainAttributesResponse
newGetIdentityMailFromDomainAttributesResponse
  pHttpStatus_ =
    GetIdentityMailFromDomainAttributesResponse'
      { httpStatus =
          pHttpStatus_,
        mailFromDomainAttributes =
          Core.mempty
      }

-- | The response's http status code.
getIdentityMailFromDomainAttributesResponse_httpStatus :: Lens.Lens' GetIdentityMailFromDomainAttributesResponse Core.Int
getIdentityMailFromDomainAttributesResponse_httpStatus = Lens.lens (\GetIdentityMailFromDomainAttributesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityMailFromDomainAttributesResponse' {} a -> s {httpStatus = a} :: GetIdentityMailFromDomainAttributesResponse)

-- | A map of identities to custom MAIL FROM attributes.
getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes :: Lens.Lens' GetIdentityMailFromDomainAttributesResponse (Core.HashMap Core.Text IdentityMailFromDomainAttributes)
getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes = Lens.lens (\GetIdentityMailFromDomainAttributesResponse' {mailFromDomainAttributes} -> mailFromDomainAttributes) (\s@GetIdentityMailFromDomainAttributesResponse' {} a -> s {mailFromDomainAttributes = a} :: GetIdentityMailFromDomainAttributesResponse) Core.. Lens._Coerce

instance
  Core.NFData
    GetIdentityMailFromDomainAttributesResponse
