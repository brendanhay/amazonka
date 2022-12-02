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
-- Module      : Amazonka.Route53.DeleteKeySigningKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a key-signing key (KSK). Before you can delete a KSK, you must
-- deactivate it. The KSK must be deactivated before you can delete it
-- regardless of whether the hosted zone is enabled for DNSSEC signing.
--
-- You can use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeactivateKeySigningKey.html DeactivateKeySigningKey>
-- to deactivate the key before you delete it.
--
-- Use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetDNSSEC.html GetDNSSEC>
-- to verify that the KSK is in an @INACTIVE@ status.
module Amazonka.Route53.DeleteKeySigningKey
  ( -- * Creating a Request
    DeleteKeySigningKey (..),
    newDeleteKeySigningKey,

    -- * Request Lenses
    deleteKeySigningKey_hostedZoneId,
    deleteKeySigningKey_name,

    -- * Destructuring the Response
    DeleteKeySigningKeyResponse (..),
    newDeleteKeySigningKeyResponse,

    -- * Response Lenses
    deleteKeySigningKeyResponse_httpStatus,
    deleteKeySigningKeyResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newDeleteKeySigningKey' smart constructor.
data DeleteKeySigningKey = DeleteKeySigningKey'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId,
    -- | A string used to identify a key-signing key (KSK).
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeySigningKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'deleteKeySigningKey_hostedZoneId' - A unique string used to identify a hosted zone.
--
-- 'name', 'deleteKeySigningKey_name' - A string used to identify a key-signing key (KSK).
newDeleteKeySigningKey ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Prelude.Text ->
  DeleteKeySigningKey
newDeleteKeySigningKey pHostedZoneId_ pName_ =
  DeleteKeySigningKey'
    { hostedZoneId = pHostedZoneId_,
      name = pName_
    }

-- | A unique string used to identify a hosted zone.
deleteKeySigningKey_hostedZoneId :: Lens.Lens' DeleteKeySigningKey ResourceId
deleteKeySigningKey_hostedZoneId = Lens.lens (\DeleteKeySigningKey' {hostedZoneId} -> hostedZoneId) (\s@DeleteKeySigningKey' {} a -> s {hostedZoneId = a} :: DeleteKeySigningKey)

-- | A string used to identify a key-signing key (KSK).
deleteKeySigningKey_name :: Lens.Lens' DeleteKeySigningKey Prelude.Text
deleteKeySigningKey_name = Lens.lens (\DeleteKeySigningKey' {name} -> name) (\s@DeleteKeySigningKey' {} a -> s {name = a} :: DeleteKeySigningKey)

instance Core.AWSRequest DeleteKeySigningKey where
  type
    AWSResponse DeleteKeySigningKey =
      DeleteKeySigningKeyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteKeySigningKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable DeleteKeySigningKey where
  hashWithSalt _salt DeleteKeySigningKey' {..} =
    _salt `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteKeySigningKey where
  rnf DeleteKeySigningKey' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteKeySigningKey where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteKeySigningKey where
  toPath DeleteKeySigningKey' {..} =
    Prelude.mconcat
      [ "/2013-04-01/keysigningkey/",
        Data.toBS hostedZoneId,
        "/",
        Data.toBS name
      ]

instance Data.ToQuery DeleteKeySigningKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKeySigningKeyResponse' smart constructor.
data DeleteKeySigningKeyResponse = DeleteKeySigningKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeySigningKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteKeySigningKeyResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'deleteKeySigningKeyResponse_changeInfo' - Undocumented member.
newDeleteKeySigningKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  DeleteKeySigningKeyResponse
newDeleteKeySigningKeyResponse
  pHttpStatus_
  pChangeInfo_ =
    DeleteKeySigningKeyResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
deleteKeySigningKeyResponse_httpStatus :: Lens.Lens' DeleteKeySigningKeyResponse Prelude.Int
deleteKeySigningKeyResponse_httpStatus = Lens.lens (\DeleteKeySigningKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteKeySigningKeyResponse' {} a -> s {httpStatus = a} :: DeleteKeySigningKeyResponse)

-- | Undocumented member.
deleteKeySigningKeyResponse_changeInfo :: Lens.Lens' DeleteKeySigningKeyResponse ChangeInfo
deleteKeySigningKeyResponse_changeInfo = Lens.lens (\DeleteKeySigningKeyResponse' {changeInfo} -> changeInfo) (\s@DeleteKeySigningKeyResponse' {} a -> s {changeInfo = a} :: DeleteKeySigningKeyResponse)

instance Prelude.NFData DeleteKeySigningKeyResponse where
  rnf DeleteKeySigningKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
