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
-- Module      : Network.AWS.Route53.DeleteKeySigningKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a key-signing key (KSK). Before you can delete a KSK, you must
-- deactivate it. The KSK must be deactived before you can delete it
-- regardless of whether the hosted zone is enabled for DNSSEC signing.
module Network.AWS.Route53.DeleteKeySigningKey
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | /See:/ 'newDeleteKeySigningKey' smart constructor.
data DeleteKeySigningKey = DeleteKeySigningKey'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId,
    -- | A string used to identify a key-signing key (KSK).
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
deleteKeySigningKey_name :: Lens.Lens' DeleteKeySigningKey Core.Text
deleteKeySigningKey_name = Lens.lens (\DeleteKeySigningKey' {name} -> name) (\s@DeleteKeySigningKey' {} a -> s {name = a} :: DeleteKeySigningKey)

instance Core.AWSRequest DeleteKeySigningKey where
  type
    AWSResponse DeleteKeySigningKey =
      DeleteKeySigningKeyResponse
  request = Request.delete defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteKeySigningKeyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "ChangeInfo")
      )

instance Core.Hashable DeleteKeySigningKey

instance Core.NFData DeleteKeySigningKey

instance Core.ToHeaders DeleteKeySigningKey where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteKeySigningKey where
  toPath DeleteKeySigningKey' {..} =
    Core.mconcat
      [ "/2013-04-01/keysigningkey/",
        Core.toBS hostedZoneId,
        "/",
        Core.toBS name
      ]

instance Core.ToQuery DeleteKeySigningKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteKeySigningKeyResponse' smart constructor.
data DeleteKeySigningKeyResponse = DeleteKeySigningKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    changeInfo :: ChangeInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
deleteKeySigningKeyResponse_httpStatus :: Lens.Lens' DeleteKeySigningKeyResponse Core.Int
deleteKeySigningKeyResponse_httpStatus = Lens.lens (\DeleteKeySigningKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteKeySigningKeyResponse' {} a -> s {httpStatus = a} :: DeleteKeySigningKeyResponse)

-- | Undocumented member.
deleteKeySigningKeyResponse_changeInfo :: Lens.Lens' DeleteKeySigningKeyResponse ChangeInfo
deleteKeySigningKeyResponse_changeInfo = Lens.lens (\DeleteKeySigningKeyResponse' {changeInfo} -> changeInfo) (\s@DeleteKeySigningKeyResponse' {} a -> s {changeInfo = a} :: DeleteKeySigningKeyResponse)

instance Core.NFData DeleteKeySigningKeyResponse
