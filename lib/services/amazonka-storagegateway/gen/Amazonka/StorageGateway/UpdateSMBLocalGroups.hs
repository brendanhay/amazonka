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
-- Module      : Amazonka.StorageGateway.UpdateSMBLocalGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the list of Active Directory users and groups that have special
-- permissions for SMB file shares on the gateway.
module Amazonka.StorageGateway.UpdateSMBLocalGroups
  ( -- * Creating a Request
    UpdateSMBLocalGroups (..),
    newUpdateSMBLocalGroups,

    -- * Request Lenses
    updateSMBLocalGroups_gatewayARN,
    updateSMBLocalGroups_sMBLocalGroups,

    -- * Destructuring the Response
    UpdateSMBLocalGroupsResponse (..),
    newUpdateSMBLocalGroupsResponse,

    -- * Response Lenses
    updateSMBLocalGroupsResponse_gatewayARN,
    updateSMBLocalGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newUpdateSMBLocalGroups' smart constructor.
data UpdateSMBLocalGroups = UpdateSMBLocalGroups'
  { gatewayARN :: Prelude.Text,
    -- | A list of Active Directory users and groups that you want to grant
    -- special permissions for SMB file shares on the gateway.
    sMBLocalGroups :: SMBLocalGroups
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSMBLocalGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateSMBLocalGroups_gatewayARN' - Undocumented member.
--
-- 'sMBLocalGroups', 'updateSMBLocalGroups_sMBLocalGroups' - A list of Active Directory users and groups that you want to grant
-- special permissions for SMB file shares on the gateway.
newUpdateSMBLocalGroups ::
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'sMBLocalGroups'
  SMBLocalGroups ->
  UpdateSMBLocalGroups
newUpdateSMBLocalGroups pGatewayARN_ pSMBLocalGroups_ =
  UpdateSMBLocalGroups'
    { gatewayARN = pGatewayARN_,
      sMBLocalGroups = pSMBLocalGroups_
    }

-- | Undocumented member.
updateSMBLocalGroups_gatewayARN :: Lens.Lens' UpdateSMBLocalGroups Prelude.Text
updateSMBLocalGroups_gatewayARN = Lens.lens (\UpdateSMBLocalGroups' {gatewayARN} -> gatewayARN) (\s@UpdateSMBLocalGroups' {} a -> s {gatewayARN = a} :: UpdateSMBLocalGroups)

-- | A list of Active Directory users and groups that you want to grant
-- special permissions for SMB file shares on the gateway.
updateSMBLocalGroups_sMBLocalGroups :: Lens.Lens' UpdateSMBLocalGroups SMBLocalGroups
updateSMBLocalGroups_sMBLocalGroups = Lens.lens (\UpdateSMBLocalGroups' {sMBLocalGroups} -> sMBLocalGroups) (\s@UpdateSMBLocalGroups' {} a -> s {sMBLocalGroups = a} :: UpdateSMBLocalGroups)

instance Core.AWSRequest UpdateSMBLocalGroups where
  type
    AWSResponse UpdateSMBLocalGroups =
      UpdateSMBLocalGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSMBLocalGroupsResponse'
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSMBLocalGroups where
  hashWithSalt _salt UpdateSMBLocalGroups' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` sMBLocalGroups

instance Prelude.NFData UpdateSMBLocalGroups where
  rnf UpdateSMBLocalGroups' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf sMBLocalGroups

instance Data.ToHeaders UpdateSMBLocalGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.UpdateSMBLocalGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSMBLocalGroups where
  toJSON UpdateSMBLocalGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just
              ("SMBLocalGroups" Data..= sMBLocalGroups)
          ]
      )

instance Data.ToPath UpdateSMBLocalGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSMBLocalGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSMBLocalGroupsResponse' smart constructor.
data UpdateSMBLocalGroupsResponse = UpdateSMBLocalGroupsResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSMBLocalGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateSMBLocalGroupsResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateSMBLocalGroupsResponse_httpStatus' - The response's http status code.
newUpdateSMBLocalGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSMBLocalGroupsResponse
newUpdateSMBLocalGroupsResponse pHttpStatus_ =
  UpdateSMBLocalGroupsResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateSMBLocalGroupsResponse_gatewayARN :: Lens.Lens' UpdateSMBLocalGroupsResponse (Prelude.Maybe Prelude.Text)
updateSMBLocalGroupsResponse_gatewayARN = Lens.lens (\UpdateSMBLocalGroupsResponse' {gatewayARN} -> gatewayARN) (\s@UpdateSMBLocalGroupsResponse' {} a -> s {gatewayARN = a} :: UpdateSMBLocalGroupsResponse)

-- | The response's http status code.
updateSMBLocalGroupsResponse_httpStatus :: Lens.Lens' UpdateSMBLocalGroupsResponse Prelude.Int
updateSMBLocalGroupsResponse_httpStatus = Lens.lens (\UpdateSMBLocalGroupsResponse' {httpStatus} -> httpStatus) (\s@UpdateSMBLocalGroupsResponse' {} a -> s {httpStatus = a} :: UpdateSMBLocalGroupsResponse)

instance Prelude.NFData UpdateSMBLocalGroupsResponse where
  rnf UpdateSMBLocalGroupsResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
