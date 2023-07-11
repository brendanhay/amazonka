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
-- Module      : Amazonka.Route53.DeactivateKeySigningKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates a key-signing key (KSK) so that it will not be used for
-- signing by DNSSEC. This operation changes the KSK status to @INACTIVE@.
module Amazonka.Route53.DeactivateKeySigningKey
  ( -- * Creating a Request
    DeactivateKeySigningKey (..),
    newDeactivateKeySigningKey,

    -- * Request Lenses
    deactivateKeySigningKey_hostedZoneId,
    deactivateKeySigningKey_name,

    -- * Destructuring the Response
    DeactivateKeySigningKeyResponse (..),
    newDeactivateKeySigningKeyResponse,

    -- * Response Lenses
    deactivateKeySigningKeyResponse_httpStatus,
    deactivateKeySigningKeyResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newDeactivateKeySigningKey' smart constructor.
data DeactivateKeySigningKey = DeactivateKeySigningKey'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId,
    -- | A string used to identify a key-signing key (KSK).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateKeySigningKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'deactivateKeySigningKey_hostedZoneId' - A unique string used to identify a hosted zone.
--
-- 'name', 'deactivateKeySigningKey_name' - A string used to identify a key-signing key (KSK).
newDeactivateKeySigningKey ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Prelude.Text ->
  DeactivateKeySigningKey
newDeactivateKeySigningKey pHostedZoneId_ pName_ =
  DeactivateKeySigningKey'
    { hostedZoneId =
        pHostedZoneId_,
      name = pName_
    }

-- | A unique string used to identify a hosted zone.
deactivateKeySigningKey_hostedZoneId :: Lens.Lens' DeactivateKeySigningKey ResourceId
deactivateKeySigningKey_hostedZoneId = Lens.lens (\DeactivateKeySigningKey' {hostedZoneId} -> hostedZoneId) (\s@DeactivateKeySigningKey' {} a -> s {hostedZoneId = a} :: DeactivateKeySigningKey)

-- | A string used to identify a key-signing key (KSK).
deactivateKeySigningKey_name :: Lens.Lens' DeactivateKeySigningKey Prelude.Text
deactivateKeySigningKey_name = Lens.lens (\DeactivateKeySigningKey' {name} -> name) (\s@DeactivateKeySigningKey' {} a -> s {name = a} :: DeactivateKeySigningKey)

instance Core.AWSRequest DeactivateKeySigningKey where
  type
    AWSResponse DeactivateKeySigningKey =
      DeactivateKeySigningKeyResponse
  request overrides =
    Request.post (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeactivateKeySigningKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable DeactivateKeySigningKey where
  hashWithSalt _salt DeactivateKeySigningKey' {..} =
    _salt
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeactivateKeySigningKey where
  rnf DeactivateKeySigningKey' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeactivateKeySigningKey where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeactivateKeySigningKey where
  toPath DeactivateKeySigningKey' {..} =
    Prelude.mconcat
      [ "/2013-04-01/keysigningkey/",
        Data.toBS hostedZoneId,
        "/",
        Data.toBS name,
        "/deactivate"
      ]

instance Data.ToQuery DeactivateKeySigningKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateKeySigningKeyResponse' smart constructor.
data DeactivateKeySigningKeyResponse = DeactivateKeySigningKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateKeySigningKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deactivateKeySigningKeyResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'deactivateKeySigningKeyResponse_changeInfo' - Undocumented member.
newDeactivateKeySigningKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  DeactivateKeySigningKeyResponse
newDeactivateKeySigningKeyResponse
  pHttpStatus_
  pChangeInfo_ =
    DeactivateKeySigningKeyResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
deactivateKeySigningKeyResponse_httpStatus :: Lens.Lens' DeactivateKeySigningKeyResponse Prelude.Int
deactivateKeySigningKeyResponse_httpStatus = Lens.lens (\DeactivateKeySigningKeyResponse' {httpStatus} -> httpStatus) (\s@DeactivateKeySigningKeyResponse' {} a -> s {httpStatus = a} :: DeactivateKeySigningKeyResponse)

-- | Undocumented member.
deactivateKeySigningKeyResponse_changeInfo :: Lens.Lens' DeactivateKeySigningKeyResponse ChangeInfo
deactivateKeySigningKeyResponse_changeInfo = Lens.lens (\DeactivateKeySigningKeyResponse' {changeInfo} -> changeInfo) (\s@DeactivateKeySigningKeyResponse' {} a -> s {changeInfo = a} :: DeactivateKeySigningKeyResponse)

instance
  Prelude.NFData
    DeactivateKeySigningKeyResponse
  where
  rnf DeactivateKeySigningKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
