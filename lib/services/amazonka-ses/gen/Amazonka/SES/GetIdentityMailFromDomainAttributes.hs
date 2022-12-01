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
-- Module      : Amazonka.SES.GetIdentityMailFromDomainAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SES.GetIdentityMailFromDomainAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to return the Amazon SES custom MAIL FROM
-- attributes for a list of identities. For information about using a
-- custom MAIL FROM domain, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide>.
--
-- /See:/ 'newGetIdentityMailFromDomainAttributes' smart constructor.
data GetIdentityMailFromDomainAttributes = GetIdentityMailFromDomainAttributes'
  { -- | A list of one or more identities.
    identities :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.mempty
    }

-- | A list of one or more identities.
getIdentityMailFromDomainAttributes_identities :: Lens.Lens' GetIdentityMailFromDomainAttributes [Prelude.Text]
getIdentityMailFromDomainAttributes_identities = Lens.lens (\GetIdentityMailFromDomainAttributes' {identities} -> identities) (\s@GetIdentityMailFromDomainAttributes' {} a -> s {identities = a} :: GetIdentityMailFromDomainAttributes) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    GetIdentityMailFromDomainAttributes
  where
  type
    AWSResponse GetIdentityMailFromDomainAttributes =
      GetIdentityMailFromDomainAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetIdentityMailFromDomainAttributesResult"
      ( \s h x ->
          GetIdentityMailFromDomainAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Core..@? "MailFromDomainAttributes"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.parseXMLMap "entry" "key" "value"
                          )
      )

instance
  Prelude.Hashable
    GetIdentityMailFromDomainAttributes
  where
  hashWithSalt
    _salt
    GetIdentityMailFromDomainAttributes' {..} =
      _salt `Prelude.hashWithSalt` identities

instance
  Prelude.NFData
    GetIdentityMailFromDomainAttributes
  where
  rnf GetIdentityMailFromDomainAttributes' {..} =
    Prelude.rnf identities

instance
  Core.ToHeaders
    GetIdentityMailFromDomainAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetIdentityMailFromDomainAttributes
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetIdentityMailFromDomainAttributes
  where
  toQuery GetIdentityMailFromDomainAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetIdentityMailFromDomainAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Identities"
          Core.=: Core.toQueryList "member" identities
      ]

-- | Represents the custom MAIL FROM attributes for a list of identities.
--
-- /See:/ 'newGetIdentityMailFromDomainAttributesResponse' smart constructor.
data GetIdentityMailFromDomainAttributesResponse = GetIdentityMailFromDomainAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A map of identities to custom MAIL FROM attributes.
    mailFromDomainAttributes :: Prelude.HashMap Prelude.Text IdentityMailFromDomainAttributes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetIdentityMailFromDomainAttributesResponse
newGetIdentityMailFromDomainAttributesResponse
  pHttpStatus_ =
    GetIdentityMailFromDomainAttributesResponse'
      { httpStatus =
          pHttpStatus_,
        mailFromDomainAttributes =
          Prelude.mempty
      }

-- | The response's http status code.
getIdentityMailFromDomainAttributesResponse_httpStatus :: Lens.Lens' GetIdentityMailFromDomainAttributesResponse Prelude.Int
getIdentityMailFromDomainAttributesResponse_httpStatus = Lens.lens (\GetIdentityMailFromDomainAttributesResponse' {httpStatus} -> httpStatus) (\s@GetIdentityMailFromDomainAttributesResponse' {} a -> s {httpStatus = a} :: GetIdentityMailFromDomainAttributesResponse)

-- | A map of identities to custom MAIL FROM attributes.
getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes :: Lens.Lens' GetIdentityMailFromDomainAttributesResponse (Prelude.HashMap Prelude.Text IdentityMailFromDomainAttributes)
getIdentityMailFromDomainAttributesResponse_mailFromDomainAttributes = Lens.lens (\GetIdentityMailFromDomainAttributesResponse' {mailFromDomainAttributes} -> mailFromDomainAttributes) (\s@GetIdentityMailFromDomainAttributesResponse' {} a -> s {mailFromDomainAttributes = a} :: GetIdentityMailFromDomainAttributesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    GetIdentityMailFromDomainAttributesResponse
  where
  rnf GetIdentityMailFromDomainAttributesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mailFromDomainAttributes
