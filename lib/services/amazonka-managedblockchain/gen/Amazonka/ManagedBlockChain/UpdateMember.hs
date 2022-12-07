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
-- Module      : Amazonka.ManagedBlockChain.UpdateMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a member configuration with new parameters.
--
-- Applies only to Hyperledger Fabric.
module Amazonka.ManagedBlockChain.UpdateMember
  ( -- * Creating a Request
    UpdateMember (..),
    newUpdateMember,

    -- * Request Lenses
    updateMember_logPublishingConfiguration,
    updateMember_networkId,
    updateMember_memberId,

    -- * Destructuring the Response
    UpdateMemberResponse (..),
    newUpdateMemberResponse,

    -- * Response Lenses
    updateMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ManagedBlockChain.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMember' smart constructor.
data UpdateMember = UpdateMember'
  { -- | Configuration properties for publishing to Amazon CloudWatch Logs.
    logPublishingConfiguration :: Prelude.Maybe MemberLogPublishingConfiguration,
    -- | The unique identifier of the Managed Blockchain network to which the
    -- member belongs.
    networkId :: Prelude.Text,
    -- | The unique identifier of the member.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPublishingConfiguration', 'updateMember_logPublishingConfiguration' - Configuration properties for publishing to Amazon CloudWatch Logs.
--
-- 'networkId', 'updateMember_networkId' - The unique identifier of the Managed Blockchain network to which the
-- member belongs.
--
-- 'memberId', 'updateMember_memberId' - The unique identifier of the member.
newUpdateMember ::
  -- | 'networkId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  UpdateMember
newUpdateMember pNetworkId_ pMemberId_ =
  UpdateMember'
    { logPublishingConfiguration =
        Prelude.Nothing,
      networkId = pNetworkId_,
      memberId = pMemberId_
    }

-- | Configuration properties for publishing to Amazon CloudWatch Logs.
updateMember_logPublishingConfiguration :: Lens.Lens' UpdateMember (Prelude.Maybe MemberLogPublishingConfiguration)
updateMember_logPublishingConfiguration = Lens.lens (\UpdateMember' {logPublishingConfiguration} -> logPublishingConfiguration) (\s@UpdateMember' {} a -> s {logPublishingConfiguration = a} :: UpdateMember)

-- | The unique identifier of the Managed Blockchain network to which the
-- member belongs.
updateMember_networkId :: Lens.Lens' UpdateMember Prelude.Text
updateMember_networkId = Lens.lens (\UpdateMember' {networkId} -> networkId) (\s@UpdateMember' {} a -> s {networkId = a} :: UpdateMember)

-- | The unique identifier of the member.
updateMember_memberId :: Lens.Lens' UpdateMember Prelude.Text
updateMember_memberId = Lens.lens (\UpdateMember' {memberId} -> memberId) (\s@UpdateMember' {} a -> s {memberId = a} :: UpdateMember)

instance Core.AWSRequest UpdateMember where
  type AWSResponse UpdateMember = UpdateMemberResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMember where
  hashWithSalt _salt UpdateMember' {..} =
    _salt
      `Prelude.hashWithSalt` logPublishingConfiguration
      `Prelude.hashWithSalt` networkId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData UpdateMember where
  rnf UpdateMember' {..} =
    Prelude.rnf logPublishingConfiguration
      `Prelude.seq` Prelude.rnf networkId
      `Prelude.seq` Prelude.rnf memberId

instance Data.ToHeaders UpdateMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMember where
  toJSON UpdateMember' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogPublishingConfiguration" Data..=)
              Prelude.<$> logPublishingConfiguration
          ]
      )

instance Data.ToPath UpdateMember where
  toPath UpdateMember' {..} =
    Prelude.mconcat
      [ "/networks/",
        Data.toBS networkId,
        "/members/",
        Data.toBS memberId
      ]

instance Data.ToQuery UpdateMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMemberResponse' smart constructor.
data UpdateMemberResponse = UpdateMemberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMemberResponse_httpStatus' - The response's http status code.
newUpdateMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMemberResponse
newUpdateMemberResponse pHttpStatus_ =
  UpdateMemberResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateMemberResponse_httpStatus :: Lens.Lens' UpdateMemberResponse Prelude.Int
updateMemberResponse_httpStatus = Lens.lens (\UpdateMemberResponse' {httpStatus} -> httpStatus) (\s@UpdateMemberResponse' {} a -> s {httpStatus = a} :: UpdateMemberResponse)

instance Prelude.NFData UpdateMemberResponse where
  rnf UpdateMemberResponse' {..} =
    Prelude.rnf httpStatus
