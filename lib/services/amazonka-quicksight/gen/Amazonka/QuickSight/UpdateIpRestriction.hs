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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the content and status of IP rules. To use this operation, you
-- need to provide the entire map of rules. You can use the
-- @DescribeIpRestriction@ operation to get the current rule map.
module Amazonka.QuickSight.UpdateIpRestriction
  ( -- * Creating a Request
    UpdateIpRestriction (..),
    newUpdateIpRestriction,

    -- * Request Lenses
    updateIpRestriction_enabled,
    updateIpRestriction_ipRestrictionRuleMap,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIpRestriction' smart constructor.
data UpdateIpRestriction = UpdateIpRestriction'
  { -- | A value that specifies whether IP rules are turned on.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A map that describes the updated IP rules with CIDR ranges and
    -- descriptions.
    ipRestrictionRuleMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the Amazon Web Services account that contains the IP rules.
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
-- 'enabled', 'updateIpRestriction_enabled' - A value that specifies whether IP rules are turned on.
--
-- 'ipRestrictionRuleMap', 'updateIpRestriction_ipRestrictionRuleMap' - A map that describes the updated IP rules with CIDR ranges and
-- descriptions.
--
-- 'awsAccountId', 'updateIpRestriction_awsAccountId' - The ID of the Amazon Web Services account that contains the IP rules.
newUpdateIpRestriction ::
  -- | 'awsAccountId'
  Prelude.Text ->
  UpdateIpRestriction
newUpdateIpRestriction pAwsAccountId_ =
  UpdateIpRestriction'
    { enabled = Prelude.Nothing,
      ipRestrictionRuleMap = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | A value that specifies whether IP rules are turned on.
updateIpRestriction_enabled :: Lens.Lens' UpdateIpRestriction (Prelude.Maybe Prelude.Bool)
updateIpRestriction_enabled = Lens.lens (\UpdateIpRestriction' {enabled} -> enabled) (\s@UpdateIpRestriction' {} a -> s {enabled = a} :: UpdateIpRestriction)

-- | A map that describes the updated IP rules with CIDR ranges and
-- descriptions.
updateIpRestriction_ipRestrictionRuleMap :: Lens.Lens' UpdateIpRestriction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIpRestriction_ipRestrictionRuleMap = Lens.lens (\UpdateIpRestriction' {ipRestrictionRuleMap} -> ipRestrictionRuleMap) (\s@UpdateIpRestriction' {} a -> s {ipRestrictionRuleMap = a} :: UpdateIpRestriction) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that contains the IP rules.
updateIpRestriction_awsAccountId :: Lens.Lens' UpdateIpRestriction Prelude.Text
updateIpRestriction_awsAccountId = Lens.lens (\UpdateIpRestriction' {awsAccountId} -> awsAccountId) (\s@UpdateIpRestriction' {} a -> s {awsAccountId = a} :: UpdateIpRestriction)

instance Core.AWSRequest UpdateIpRestriction where
  type
    AWSResponse UpdateIpRestriction =
      UpdateIpRestrictionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIpRestrictionResponse'
            Prelude.<$> (x Data..?> "AwsAccountId")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIpRestriction where
  hashWithSalt _salt UpdateIpRestriction' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` ipRestrictionRuleMap
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData UpdateIpRestriction where
  rnf UpdateIpRestriction' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf ipRestrictionRuleMap
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders UpdateIpRestriction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIpRestriction where
  toJSON UpdateIpRestriction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("IpRestrictionRuleMap" Data..=)
              Prelude.<$> ipRestrictionRuleMap
          ]
      )

instance Data.ToPath UpdateIpRestriction where
  toPath UpdateIpRestriction' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/ip-restriction"
      ]

instance Data.ToQuery UpdateIpRestriction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIpRestrictionResponse' smart constructor.
data UpdateIpRestrictionResponse = UpdateIpRestrictionResponse'
  { -- | The ID of the Amazon Web Services account that contains the IP rules.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
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
-- 'awsAccountId', 'updateIpRestrictionResponse_awsAccountId' - The ID of the Amazon Web Services account that contains the IP rules.
--
-- 'requestId', 'updateIpRestrictionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'updateIpRestrictionResponse_status' - The HTTP status of the request.
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

-- | The ID of the Amazon Web Services account that contains the IP rules.
updateIpRestrictionResponse_awsAccountId :: Lens.Lens' UpdateIpRestrictionResponse (Prelude.Maybe Prelude.Text)
updateIpRestrictionResponse_awsAccountId = Lens.lens (\UpdateIpRestrictionResponse' {awsAccountId} -> awsAccountId) (\s@UpdateIpRestrictionResponse' {} a -> s {awsAccountId = a} :: UpdateIpRestrictionResponse)

-- | The Amazon Web Services request ID for this operation.
updateIpRestrictionResponse_requestId :: Lens.Lens' UpdateIpRestrictionResponse (Prelude.Maybe Prelude.Text)
updateIpRestrictionResponse_requestId = Lens.lens (\UpdateIpRestrictionResponse' {requestId} -> requestId) (\s@UpdateIpRestrictionResponse' {} a -> s {requestId = a} :: UpdateIpRestrictionResponse)

-- | The HTTP status of the request.
updateIpRestrictionResponse_status :: Lens.Lens' UpdateIpRestrictionResponse Prelude.Int
updateIpRestrictionResponse_status = Lens.lens (\UpdateIpRestrictionResponse' {status} -> status) (\s@UpdateIpRestrictionResponse' {} a -> s {status = a} :: UpdateIpRestrictionResponse)

instance Prelude.NFData UpdateIpRestrictionResponse where
  rnf UpdateIpRestrictionResponse' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
