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
-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a list of identities (email addresses and\/or domains), returns
-- the verification status and (for domain identities) the verification
-- token for each identity.
--
-- The verification status of an email address is \"Pending\" until the
-- email address owner clicks the link within the verification email that
-- Amazon SES sent to that address. If the email address owner clicks the
-- link within 24 hours, the verification status of the email address
-- changes to \"Success\". If the link is not clicked within 24 hours, the
-- verification status changes to \"Failed.\" In that case, if you still
-- want to verify the email address, you must restart the verification
-- process from the beginning.
--
-- For domain identities, the domain\'s verification status is \"Pending\"
-- as Amazon SES searches for the required TXT record in the DNS settings
-- of the domain. When Amazon SES detects the record, the domain\'s
-- verification status changes to \"Success\". If Amazon SES is unable to
-- detect the record within 72 hours, the domain\'s verification status
-- changes to \"Failed.\" In that case, if you still want to verify the
-- domain, you must restart the verification process from the beginning.
--
-- This operation is throttled at one request per second and can only get
-- verification attributes for up to 100 identities at a time.
module Network.AWS.SES.GetIdentityVerificationAttributes
  ( -- * Creating a Request
    GetIdentityVerificationAttributes (..),
    newGetIdentityVerificationAttributes,

    -- * Request Lenses
    getIdentityVerificationAttributes_identities,

    -- * Destructuring the Response
    GetIdentityVerificationAttributesResponse (..),
    newGetIdentityVerificationAttributesResponse,

    -- * Response Lenses
    getIdentityVerificationAttributesResponse_httpStatus,
    getIdentityVerificationAttributesResponse_verificationAttributes,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to return the Amazon SES verification status of a
-- list of identities. For domain identities, this request also returns the
-- verification token. For information about verifying identities with
-- Amazon SES, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide>.
--
-- /See:/ 'newGetIdentityVerificationAttributes' smart constructor.
data GetIdentityVerificationAttributes = GetIdentityVerificationAttributes'
  { -- | A list of identities.
    identities :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityVerificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identities', 'getIdentityVerificationAttributes_identities' - A list of identities.
newGetIdentityVerificationAttributes ::
  GetIdentityVerificationAttributes
newGetIdentityVerificationAttributes =
  GetIdentityVerificationAttributes'
    { identities =
        Core.mempty
    }

-- | A list of identities.
getIdentityVerificationAttributes_identities :: Lens.Lens' GetIdentityVerificationAttributes [Core.Text]
getIdentityVerificationAttributes_identities = Lens.lens (\GetIdentityVerificationAttributes' {identities} -> identities) (\s@GetIdentityVerificationAttributes' {} a -> s {identities = a} :: GetIdentityVerificationAttributes) Core.. Lens._Coerce

instance
  Core.AWSRequest
    GetIdentityVerificationAttributes
  where
  type
    AWSResponse GetIdentityVerificationAttributes =
      GetIdentityVerificationAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetIdentityVerificationAttributesResult"
      ( \s h x ->
          GetIdentityVerificationAttributesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "VerificationAttributes"
                         Core..!@ Core.mempty
                         Core.>>= Core.parseXMLMap "entry" "key" "value"
                     )
      )

instance
  Core.Hashable
    GetIdentityVerificationAttributes

instance
  Core.NFData
    GetIdentityVerificationAttributes

instance
  Core.ToHeaders
    GetIdentityVerificationAttributes
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetIdentityVerificationAttributes
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetIdentityVerificationAttributes
  where
  toQuery GetIdentityVerificationAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetIdentityVerificationAttributes" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "Identities"
          Core.=: Core.toQueryList "member" identities
      ]

-- | The Amazon SES verification status of a list of identities. For domain
-- identities, this response also contains the verification token.
--
-- /See:/ 'newGetIdentityVerificationAttributesResponse' smart constructor.
data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A map of Identities to IdentityVerificationAttributes objects.
    verificationAttributes :: Core.HashMap Core.Text IdentityVerificationAttributes
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetIdentityVerificationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getIdentityVerificationAttributesResponse_httpStatus' - The response's http status code.
--
-- 'verificationAttributes', 'getIdentityVerificationAttributesResponse_verificationAttributes' - A map of Identities to IdentityVerificationAttributes objects.
newGetIdentityVerificationAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetIdentityVerificationAttributesResponse
newGetIdentityVerificationAttributesResponse
  pHttpStatus_ =
    GetIdentityVerificationAttributesResponse'
      { httpStatus =
          pHttpStatus_,
        verificationAttributes =
          Core.mempty
      }

-- | The response's http status code.
getIdentityVerificationAttributesResponse_httpStatus :: Lens.Lens' GetIdentityVerificationAttributesResponse Core.Int
getIdentityVerificationAttributesResponse_httpStatus = Lens.lens (\GetIdentityVerificationAttributesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityVerificationAttributesResponse' {} a -> s {httpStatus = a} :: GetIdentityVerificationAttributesResponse)

-- | A map of Identities to IdentityVerificationAttributes objects.
getIdentityVerificationAttributesResponse_verificationAttributes :: Lens.Lens' GetIdentityVerificationAttributesResponse (Core.HashMap Core.Text IdentityVerificationAttributes)
getIdentityVerificationAttributesResponse_verificationAttributes = Lens.lens (\GetIdentityVerificationAttributesResponse' {verificationAttributes} -> verificationAttributes) (\s@GetIdentityVerificationAttributesResponse' {} a -> s {verificationAttributes = a} :: GetIdentityVerificationAttributesResponse) Core.. Lens._Coerce

instance
  Core.NFData
    GetIdentityVerificationAttributesResponse
