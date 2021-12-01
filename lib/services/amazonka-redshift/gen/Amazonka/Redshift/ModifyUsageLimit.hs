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
-- Module      : Amazonka.Redshift.ModifyUsageLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a usage limit in a cluster. You can\'t modify the feature type
-- or period of a usage limit.
module Amazonka.Redshift.ModifyUsageLimit
  ( -- * Creating a Request
    ModifyUsageLimit (..),
    newModifyUsageLimit,

    -- * Request Lenses
    modifyUsageLimit_amount,
    modifyUsageLimit_breachAction,
    modifyUsageLimit_usageLimitId,

    -- * Destructuring the Response
    UsageLimit (..),
    newUsageLimit,

    -- * Response Lenses
    usageLimit_amount,
    usageLimit_limitType,
    usageLimit_usageLimitId,
    usageLimit_period,
    usageLimit_clusterIdentifier,
    usageLimit_breachAction,
    usageLimit_featureType,
    usageLimit_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyUsageLimit' smart constructor.
data ModifyUsageLimit = ModifyUsageLimit'
  { -- | The new limit amount. For more information about this parameter, see
    -- UsageLimit.
    amount :: Prelude.Maybe Prelude.Integer,
    -- | The new action that Amazon Redshift takes when the limit is reached. For
    -- more information about this parameter, see UsageLimit.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
    -- | The identifier of the usage limit to modify.
    usageLimitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'modifyUsageLimit_amount' - The new limit amount. For more information about this parameter, see
-- UsageLimit.
--
-- 'breachAction', 'modifyUsageLimit_breachAction' - The new action that Amazon Redshift takes when the limit is reached. For
-- more information about this parameter, see UsageLimit.
--
-- 'usageLimitId', 'modifyUsageLimit_usageLimitId' - The identifier of the usage limit to modify.
newModifyUsageLimit ::
  -- | 'usageLimitId'
  Prelude.Text ->
  ModifyUsageLimit
newModifyUsageLimit pUsageLimitId_ =
  ModifyUsageLimit'
    { amount = Prelude.Nothing,
      breachAction = Prelude.Nothing,
      usageLimitId = pUsageLimitId_
    }

-- | The new limit amount. For more information about this parameter, see
-- UsageLimit.
modifyUsageLimit_amount :: Lens.Lens' ModifyUsageLimit (Prelude.Maybe Prelude.Integer)
modifyUsageLimit_amount = Lens.lens (\ModifyUsageLimit' {amount} -> amount) (\s@ModifyUsageLimit' {} a -> s {amount = a} :: ModifyUsageLimit)

-- | The new action that Amazon Redshift takes when the limit is reached. For
-- more information about this parameter, see UsageLimit.
modifyUsageLimit_breachAction :: Lens.Lens' ModifyUsageLimit (Prelude.Maybe UsageLimitBreachAction)
modifyUsageLimit_breachAction = Lens.lens (\ModifyUsageLimit' {breachAction} -> breachAction) (\s@ModifyUsageLimit' {} a -> s {breachAction = a} :: ModifyUsageLimit)

-- | The identifier of the usage limit to modify.
modifyUsageLimit_usageLimitId :: Lens.Lens' ModifyUsageLimit Prelude.Text
modifyUsageLimit_usageLimitId = Lens.lens (\ModifyUsageLimit' {usageLimitId} -> usageLimitId) (\s@ModifyUsageLimit' {} a -> s {usageLimitId = a} :: ModifyUsageLimit)

instance Core.AWSRequest ModifyUsageLimit where
  type AWSResponse ModifyUsageLimit = UsageLimit
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyUsageLimitResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable ModifyUsageLimit where
  hashWithSalt salt' ModifyUsageLimit' {..} =
    salt' `Prelude.hashWithSalt` usageLimitId
      `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` amount

instance Prelude.NFData ModifyUsageLimit where
  rnf ModifyUsageLimit' {..} =
    Prelude.rnf amount
      `Prelude.seq` Prelude.rnf usageLimitId
      `Prelude.seq` Prelude.rnf breachAction

instance Core.ToHeaders ModifyUsageLimit where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyUsageLimit where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyUsageLimit where
  toQuery ModifyUsageLimit' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyUsageLimit" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "Amount" Core.=: amount,
        "BreachAction" Core.=: breachAction,
        "UsageLimitId" Core.=: usageLimitId
      ]
