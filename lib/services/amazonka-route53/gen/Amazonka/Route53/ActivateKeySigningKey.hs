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
-- Module      : Amazonka.Route53.ActivateKeySigningKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a key-signing key (KSK) so that it can be used for signing by
-- DNSSEC. This operation changes the KSK status to @ACTIVE@.
module Amazonka.Route53.ActivateKeySigningKey
  ( -- * Creating a Request
    ActivateKeySigningKey (..),
    newActivateKeySigningKey,

    -- * Request Lenses
    activateKeySigningKey_hostedZoneId,
    activateKeySigningKey_name,

    -- * Destructuring the Response
    ActivateKeySigningKeyResponse (..),
    newActivateKeySigningKeyResponse,

    -- * Response Lenses
    activateKeySigningKeyResponse_httpStatus,
    activateKeySigningKeyResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newActivateKeySigningKey' smart constructor.
data ActivateKeySigningKey = ActivateKeySigningKey'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId,
    -- | A string used to identify a key-signing key (KSK). @Name@ can include
    -- numbers, letters, and underscores (_). @Name@ must be unique for each
    -- key-signing key in the same hosted zone.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateKeySigningKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'activateKeySigningKey_hostedZoneId' - A unique string used to identify a hosted zone.
--
-- 'name', 'activateKeySigningKey_name' - A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
newActivateKeySigningKey ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Prelude.Text ->
  ActivateKeySigningKey
newActivateKeySigningKey pHostedZoneId_ pName_ =
  ActivateKeySigningKey'
    { hostedZoneId =
        pHostedZoneId_,
      name = pName_
    }

-- | A unique string used to identify a hosted zone.
activateKeySigningKey_hostedZoneId :: Lens.Lens' ActivateKeySigningKey ResourceId
activateKeySigningKey_hostedZoneId = Lens.lens (\ActivateKeySigningKey' {hostedZoneId} -> hostedZoneId) (\s@ActivateKeySigningKey' {} a -> s {hostedZoneId = a} :: ActivateKeySigningKey)

-- | A string used to identify a key-signing key (KSK). @Name@ can include
-- numbers, letters, and underscores (_). @Name@ must be unique for each
-- key-signing key in the same hosted zone.
activateKeySigningKey_name :: Lens.Lens' ActivateKeySigningKey Prelude.Text
activateKeySigningKey_name = Lens.lens (\ActivateKeySigningKey' {name} -> name) (\s@ActivateKeySigningKey' {} a -> s {name = a} :: ActivateKeySigningKey)

instance Core.AWSRequest ActivateKeySigningKey where
  type
    AWSResponse ActivateKeySigningKey =
      ActivateKeySigningKeyResponse
  request overrides =
    Request.post (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ActivateKeySigningKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable ActivateKeySigningKey where
  hashWithSalt _salt ActivateKeySigningKey' {..} =
    _salt
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` name

instance Prelude.NFData ActivateKeySigningKey where
  rnf ActivateKeySigningKey' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders ActivateKeySigningKey where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ActivateKeySigningKey where
  toPath ActivateKeySigningKey' {..} =
    Prelude.mconcat
      [ "/2013-04-01/keysigningkey/",
        Data.toBS hostedZoneId,
        "/",
        Data.toBS name,
        "/activate"
      ]

instance Data.ToQuery ActivateKeySigningKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newActivateKeySigningKeyResponse' smart constructor.
data ActivateKeySigningKeyResponse = ActivateKeySigningKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateKeySigningKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'activateKeySigningKeyResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'activateKeySigningKeyResponse_changeInfo' - Undocumented member.
newActivateKeySigningKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  ActivateKeySigningKeyResponse
newActivateKeySigningKeyResponse
  pHttpStatus_
  pChangeInfo_ =
    ActivateKeySigningKeyResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
activateKeySigningKeyResponse_httpStatus :: Lens.Lens' ActivateKeySigningKeyResponse Prelude.Int
activateKeySigningKeyResponse_httpStatus = Lens.lens (\ActivateKeySigningKeyResponse' {httpStatus} -> httpStatus) (\s@ActivateKeySigningKeyResponse' {} a -> s {httpStatus = a} :: ActivateKeySigningKeyResponse)

-- | Undocumented member.
activateKeySigningKeyResponse_changeInfo :: Lens.Lens' ActivateKeySigningKeyResponse ChangeInfo
activateKeySigningKeyResponse_changeInfo = Lens.lens (\ActivateKeySigningKeyResponse' {changeInfo} -> changeInfo) (\s@ActivateKeySigningKeyResponse' {} a -> s {changeInfo = a} :: ActivateKeySigningKeyResponse)

instance Prelude.NFData ActivateKeySigningKeyResponse where
  rnf ActivateKeySigningKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
