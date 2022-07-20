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
-- Module      : Amazonka.QuickSight.UpdateIpRestriction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates content and status of IP Rules.
module Amazonka.QuickSight.UpdateIpRestriction
  ( -- * Creating a Request
    UpdateIpRestriction (..),
    newUpdateIpRestriction,

    -- * Request Lenses
    updateIpRestriction_ipRestrictionRuleMap,
    updateIpRestriction_enabled,
    updateIpRestriction_awsAccountId,

    -- * Destructuring the Response
    UpdateIpRestrictionResponse (..),
    newUpdateIpRestrictionResponse,

    -- * Response Lenses
    updateIpRestrictionResponse_awsAccountId,
    updateIpRestrictionResponse_requestId,
    updateIpRestrictionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIpRestriction' smart constructor.
data UpdateIpRestriction = UpdateIpRestriction'
  { -- | Describes updated IP rules.
    ipRestrictionRuleMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Whether or not IP rules are enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Your AWS account ID.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIpRestriction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipRestrictionRuleMap', 'updateIpRestriction_ipRestrictionRuleMap' - Describes updated IP rules.
--
-- 'enabled', 'updateIpRestriction_enabled' - Whether or not IP rules are enabled.
--
-- 'awsAccountId', 'updateIpRestriction_awsAccountId' - Your AWS account ID.
newUpdateIpRestriction ::
  -- | 'awsAccountId'
  Prelude.Text ->
  UpdateIpRestriction
newUpdateIpRestriction pAwsAccountId_ =
  UpdateIpRestriction'
    { ipRestrictionRuleMap =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | Describes updated IP rules.
updateIpRestriction_ipRestrictionRuleMap :: Lens.Lens' UpdateIpRestriction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIpRestriction_ipRestrictionRuleMap = Lens.lens (\UpdateIpRestriction' {ipRestrictionRuleMap} -> ipRestrictionRuleMap) (\s@UpdateIpRestriction' {} a -> s {ipRestrictionRuleMap = a} :: UpdateIpRestriction) Prelude.. Lens.mapping Lens.coerced

-- | Whether or not IP rules are enabled.
updateIpRestriction_enabled :: Lens.Lens' UpdateIpRestriction (Prelude.Maybe Prelude.Bool)
updateIpRestriction_enabled = Lens.lens (\UpdateIpRestriction' {enabled} -> enabled) (\s@UpdateIpRestriction' {} a -> s {enabled = a} :: UpdateIpRestriction)

-- | Your AWS account ID.
updateIpRestriction_awsAccountId :: Lens.Lens' UpdateIpRestriction Prelude.Text
updateIpRestriction_awsAccountId = Lens.lens (\UpdateIpRestriction' {awsAccountId} -> awsAccountId) (\s@UpdateIpRestriction' {} a -> s {awsAccountId = a} :: UpdateIpRestriction)

instance Core.AWSRequest UpdateIpRestriction where
  type
    AWSResponse UpdateIpRestriction =
      UpdateIpRestrictionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIpRestrictionResponse'
            Prelude.<$> (x Core..?> "AwsAccountId")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIpRestriction where
  hashWithSalt _salt UpdateIpRestriction' {..} =
    _salt `Prelude.hashWithSalt` ipRestrictionRuleMap
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData UpdateIpRestriction where
  rnf UpdateIpRestriction' {..} =
    Prelude.rnf ipRestrictionRuleMap
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf awsAccountId

instance Core.ToHeaders UpdateIpRestriction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateIpRestriction where
  toJSON UpdateIpRestriction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IpRestrictionRuleMap" Core..=)
              Prelude.<$> ipRestrictionRuleMap,
            ("Enabled" Core..=) Prelude.<$> enabled
          ]
      )

instance Core.ToPath UpdateIpRestriction where
  toPath UpdateIpRestriction' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/ip-restriction"
      ]

instance Core.ToQuery UpdateIpRestriction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIpRestrictionResponse' smart constructor.
data UpdateIpRestrictionResponse = UpdateIpRestrictionResponse'
  { -- | Your AWS account ID.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the update request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The status of the updated IP rules. A successful request returns a 200
    -- code.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIpRestrictionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'updateIpRestrictionResponse_awsAccountId' - Your AWS account ID.
--
-- 'requestId', 'updateIpRestrictionResponse_requestId' - The ID of the update request.
--
-- 'status', 'updateIpRestrictionResponse_status' - The status of the updated IP rules. A successful request returns a 200
-- code.
newUpdateIpRestrictionResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateIpRestrictionResponse
newUpdateIpRestrictionResponse pStatus_ =
  UpdateIpRestrictionResponse'
    { awsAccountId =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Your AWS account ID.
updateIpRestrictionResponse_awsAccountId :: Lens.Lens' UpdateIpRestrictionResponse (Prelude.Maybe Prelude.Text)
updateIpRestrictionResponse_awsAccountId = Lens.lens (\UpdateIpRestrictionResponse' {awsAccountId} -> awsAccountId) (\s@UpdateIpRestrictionResponse' {} a -> s {awsAccountId = a} :: UpdateIpRestrictionResponse)

-- | The ID of the update request.
updateIpRestrictionResponse_requestId :: Lens.Lens' UpdateIpRestrictionResponse (Prelude.Maybe Prelude.Text)
updateIpRestrictionResponse_requestId = Lens.lens (\UpdateIpRestrictionResponse' {requestId} -> requestId) (\s@UpdateIpRestrictionResponse' {} a -> s {requestId = a} :: UpdateIpRestrictionResponse)

-- | The status of the updated IP rules. A successful request returns a 200
-- code.
updateIpRestrictionResponse_status :: Lens.Lens' UpdateIpRestrictionResponse Prelude.Int
updateIpRestrictionResponse_status = Lens.lens (\UpdateIpRestrictionResponse' {status} -> status) (\s@UpdateIpRestrictionResponse' {} a -> s {status = a} :: UpdateIpRestrictionResponse)

instance Prelude.NFData UpdateIpRestrictionResponse where
  rnf UpdateIpRestrictionResponse' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
