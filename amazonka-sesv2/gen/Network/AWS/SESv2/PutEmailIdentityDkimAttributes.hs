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
-- Module      : Network.AWS.SESv2.PutEmailIdentityDkimAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to enable or disable DKIM authentication for an email identity.
module Network.AWS.SESv2.PutEmailIdentityDkimAttributes
  ( -- * Creating a Request
    PutEmailIdentityDkimAttributes (..),
    newPutEmailIdentityDkimAttributes,

    -- * Request Lenses
    putEmailIdentityDkimAttributes_signingEnabled,
    putEmailIdentityDkimAttributes_emailIdentity,

    -- * Destructuring the Response
    PutEmailIdentityDkimAttributesResponse (..),
    newPutEmailIdentityDkimAttributesResponse,

    -- * Response Lenses
    putEmailIdentityDkimAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to enable or disable DKIM signing of email that you send from
-- an email identity.
--
-- /See:/ 'newPutEmailIdentityDkimAttributes' smart constructor.
data PutEmailIdentityDkimAttributes = PutEmailIdentityDkimAttributes'
  { -- | Sets the DKIM signing configuration for the identity.
    --
    -- When you set this value @true@, then the messages that are sent from the
    -- identity are signed using DKIM. If you set this value to @false@, your
    -- messages are sent without DKIM signing.
    signingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The email identity that you want to change the DKIM settings for.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityDkimAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingEnabled', 'putEmailIdentityDkimAttributes_signingEnabled' - Sets the DKIM signing configuration for the identity.
--
-- When you set this value @true@, then the messages that are sent from the
-- identity are signed using DKIM. If you set this value to @false@, your
-- messages are sent without DKIM signing.
--
-- 'emailIdentity', 'putEmailIdentityDkimAttributes_emailIdentity' - The email identity that you want to change the DKIM settings for.
newPutEmailIdentityDkimAttributes ::
  -- | 'emailIdentity'
  Prelude.Text ->
  PutEmailIdentityDkimAttributes
newPutEmailIdentityDkimAttributes pEmailIdentity_ =
  PutEmailIdentityDkimAttributes'
    { signingEnabled =
        Prelude.Nothing,
      emailIdentity = pEmailIdentity_
    }

-- | Sets the DKIM signing configuration for the identity.
--
-- When you set this value @true@, then the messages that are sent from the
-- identity are signed using DKIM. If you set this value to @false@, your
-- messages are sent without DKIM signing.
putEmailIdentityDkimAttributes_signingEnabled :: Lens.Lens' PutEmailIdentityDkimAttributes (Prelude.Maybe Prelude.Bool)
putEmailIdentityDkimAttributes_signingEnabled = Lens.lens (\PutEmailIdentityDkimAttributes' {signingEnabled} -> signingEnabled) (\s@PutEmailIdentityDkimAttributes' {} a -> s {signingEnabled = a} :: PutEmailIdentityDkimAttributes)

-- | The email identity that you want to change the DKIM settings for.
putEmailIdentityDkimAttributes_emailIdentity :: Lens.Lens' PutEmailIdentityDkimAttributes Prelude.Text
putEmailIdentityDkimAttributes_emailIdentity = Lens.lens (\PutEmailIdentityDkimAttributes' {emailIdentity} -> emailIdentity) (\s@PutEmailIdentityDkimAttributes' {} a -> s {emailIdentity = a} :: PutEmailIdentityDkimAttributes)

instance
  Core.AWSRequest
    PutEmailIdentityDkimAttributes
  where
  type
    AWSResponse PutEmailIdentityDkimAttributes =
      PutEmailIdentityDkimAttributesResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutEmailIdentityDkimAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutEmailIdentityDkimAttributes

instance
  Prelude.NFData
    PutEmailIdentityDkimAttributes

instance
  Core.ToHeaders
    PutEmailIdentityDkimAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutEmailIdentityDkimAttributes where
  toJSON PutEmailIdentityDkimAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SigningEnabled" Core..=)
              Prelude.<$> signingEnabled
          ]
      )

instance Core.ToPath PutEmailIdentityDkimAttributes where
  toPath PutEmailIdentityDkimAttributes' {..} =
    Prelude.mconcat
      [ "/v2/email/identities/",
        Core.toBS emailIdentity,
        "/dkim"
      ]

instance Core.ToQuery PutEmailIdentityDkimAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutEmailIdentityDkimAttributesResponse' smart constructor.
data PutEmailIdentityDkimAttributesResponse = PutEmailIdentityDkimAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityDkimAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEmailIdentityDkimAttributesResponse_httpStatus' - The response's http status code.
newPutEmailIdentityDkimAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEmailIdentityDkimAttributesResponse
newPutEmailIdentityDkimAttributesResponse
  pHttpStatus_ =
    PutEmailIdentityDkimAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putEmailIdentityDkimAttributesResponse_httpStatus :: Lens.Lens' PutEmailIdentityDkimAttributesResponse Prelude.Int
putEmailIdentityDkimAttributesResponse_httpStatus = Lens.lens (\PutEmailIdentityDkimAttributesResponse' {httpStatus} -> httpStatus) (\s@PutEmailIdentityDkimAttributesResponse' {} a -> s {httpStatus = a} :: PutEmailIdentityDkimAttributesResponse)

instance
  Prelude.NFData
    PutEmailIdentityDkimAttributesResponse
