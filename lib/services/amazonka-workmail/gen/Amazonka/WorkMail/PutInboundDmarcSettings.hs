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
-- Module      : Amazonka.WorkMail.PutInboundDmarcSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables a DMARC policy for a given organization.
module Amazonka.WorkMail.PutInboundDmarcSettings
  ( -- * Creating a Request
    PutInboundDmarcSettings (..),
    newPutInboundDmarcSettings,

    -- * Request Lenses
    putInboundDmarcSettings_organizationId,
    putInboundDmarcSettings_enforced,

    -- * Destructuring the Response
    PutInboundDmarcSettingsResponse (..),
    newPutInboundDmarcSettingsResponse,

    -- * Response Lenses
    putInboundDmarcSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newPutInboundDmarcSettings' smart constructor.
data PutInboundDmarcSettings = PutInboundDmarcSettings'
  { -- | The ID of the organization that you are applying the DMARC policy to.
    organizationId :: Prelude.Text,
    -- | Enforces or suspends a policy after it\'s applied.
    enforced :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInboundDmarcSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'putInboundDmarcSettings_organizationId' - The ID of the organization that you are applying the DMARC policy to.
--
-- 'enforced', 'putInboundDmarcSettings_enforced' - Enforces or suspends a policy after it\'s applied.
newPutInboundDmarcSettings ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'enforced'
  Prelude.Bool ->
  PutInboundDmarcSettings
newPutInboundDmarcSettings
  pOrganizationId_
  pEnforced_ =
    PutInboundDmarcSettings'
      { organizationId =
          pOrganizationId_,
        enforced = pEnforced_
      }

-- | The ID of the organization that you are applying the DMARC policy to.
putInboundDmarcSettings_organizationId :: Lens.Lens' PutInboundDmarcSettings Prelude.Text
putInboundDmarcSettings_organizationId = Lens.lens (\PutInboundDmarcSettings' {organizationId} -> organizationId) (\s@PutInboundDmarcSettings' {} a -> s {organizationId = a} :: PutInboundDmarcSettings)

-- | Enforces or suspends a policy after it\'s applied.
putInboundDmarcSettings_enforced :: Lens.Lens' PutInboundDmarcSettings Prelude.Bool
putInboundDmarcSettings_enforced = Lens.lens (\PutInboundDmarcSettings' {enforced} -> enforced) (\s@PutInboundDmarcSettings' {} a -> s {enforced = a} :: PutInboundDmarcSettings)

instance Core.AWSRequest PutInboundDmarcSettings where
  type
    AWSResponse PutInboundDmarcSettings =
      PutInboundDmarcSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutInboundDmarcSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutInboundDmarcSettings where
  hashWithSalt _salt PutInboundDmarcSettings' {..} =
    _salt
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` enforced

instance Prelude.NFData PutInboundDmarcSettings where
  rnf PutInboundDmarcSettings' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf enforced

instance Data.ToHeaders PutInboundDmarcSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.PutInboundDmarcSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutInboundDmarcSettings where
  toJSON PutInboundDmarcSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("Enforced" Data..= enforced)
          ]
      )

instance Data.ToPath PutInboundDmarcSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery PutInboundDmarcSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutInboundDmarcSettingsResponse' smart constructor.
data PutInboundDmarcSettingsResponse = PutInboundDmarcSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInboundDmarcSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putInboundDmarcSettingsResponse_httpStatus' - The response's http status code.
newPutInboundDmarcSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutInboundDmarcSettingsResponse
newPutInboundDmarcSettingsResponse pHttpStatus_ =
  PutInboundDmarcSettingsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putInboundDmarcSettingsResponse_httpStatus :: Lens.Lens' PutInboundDmarcSettingsResponse Prelude.Int
putInboundDmarcSettingsResponse_httpStatus = Lens.lens (\PutInboundDmarcSettingsResponse' {httpStatus} -> httpStatus) (\s@PutInboundDmarcSettingsResponse' {} a -> s {httpStatus = a} :: PutInboundDmarcSettingsResponse)

instance
  Prelude.NFData
    PutInboundDmarcSettingsResponse
  where
  rnf PutInboundDmarcSettingsResponse' {..} =
    Prelude.rnf httpStatus
