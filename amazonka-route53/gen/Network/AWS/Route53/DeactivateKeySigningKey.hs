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
-- Module      : Network.AWS.Route53.DeactivateKeySigningKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates a key-signing key (KSK) so that it will not be used for
-- signing by DNSSEC. This operation changes the KSK status to @INACTIVE@.
module Network.AWS.Route53.DeactivateKeySigningKey
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | /See:/ 'newDeactivateKeySigningKey' smart constructor.
data DeactivateKeySigningKey = DeactivateKeySigningKey'
  { -- | A unique string used to identify a hosted zone.
    hostedZoneId :: ResourceId,
    -- | A string used to identify a key-signing key (KSK).
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
deactivateKeySigningKey_name :: Lens.Lens' DeactivateKeySigningKey Core.Text
deactivateKeySigningKey_name = Lens.lens (\DeactivateKeySigningKey' {name} -> name) (\s@DeactivateKeySigningKey' {} a -> s {name = a} :: DeactivateKeySigningKey)

instance Core.AWSRequest DeactivateKeySigningKey where
  type
    AWSResponse DeactivateKeySigningKey =
      DeactivateKeySigningKeyResponse
  request = Request.post defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeactivateKeySigningKeyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "ChangeInfo")
      )

instance Core.Hashable DeactivateKeySigningKey

instance Core.NFData DeactivateKeySigningKey

instance Core.ToHeaders DeactivateKeySigningKey where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeactivateKeySigningKey where
  toPath DeactivateKeySigningKey' {..} =
    Core.mconcat
      [ "/2013-04-01/keysigningkey/",
        Core.toBS hostedZoneId,
        "/",
        Core.toBS name,
        "/deactivate"
      ]

instance Core.ToQuery DeactivateKeySigningKey where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeactivateKeySigningKeyResponse' smart constructor.
data DeactivateKeySigningKeyResponse = DeactivateKeySigningKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    changeInfo :: ChangeInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
deactivateKeySigningKeyResponse_httpStatus :: Lens.Lens' DeactivateKeySigningKeyResponse Core.Int
deactivateKeySigningKeyResponse_httpStatus = Lens.lens (\DeactivateKeySigningKeyResponse' {httpStatus} -> httpStatus) (\s@DeactivateKeySigningKeyResponse' {} a -> s {httpStatus = a} :: DeactivateKeySigningKeyResponse)

-- | Undocumented member.
deactivateKeySigningKeyResponse_changeInfo :: Lens.Lens' DeactivateKeySigningKeyResponse ChangeInfo
deactivateKeySigningKeyResponse_changeInfo = Lens.lens (\DeactivateKeySigningKeyResponse' {changeInfo} -> changeInfo) (\s@DeactivateKeySigningKeyResponse' {} a -> s {changeInfo = a} :: DeactivateKeySigningKeyResponse)

instance Core.NFData DeactivateKeySigningKeyResponse
