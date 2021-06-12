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
-- Module      : Network.AWS.Redshift.ModifyUsageLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a usage limit in a cluster. You can\'t modify the feature type
-- or period of a usage limit.
module Network.AWS.Redshift.ModifyUsageLimit
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
    usageLimit_featureType,
    usageLimit_breachAction,
    usageLimit_limitType,
    usageLimit_clusterIdentifier,
    usageLimit_tags,
    usageLimit_period,
    usageLimit_usageLimitId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyUsageLimit' smart constructor.
data ModifyUsageLimit = ModifyUsageLimit'
  { -- | The new limit amount. For more information about this parameter, see
    -- UsageLimit.
    amount :: Core.Maybe Core.Integer,
    -- | The new action that Amazon Redshift takes when the limit is reached. For
    -- more information about this parameter, see UsageLimit.
    breachAction :: Core.Maybe UsageLimitBreachAction,
    -- | The identifier of the usage limit to modify.
    usageLimitId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ModifyUsageLimit
newModifyUsageLimit pUsageLimitId_ =
  ModifyUsageLimit'
    { amount = Core.Nothing,
      breachAction = Core.Nothing,
      usageLimitId = pUsageLimitId_
    }

-- | The new limit amount. For more information about this parameter, see
-- UsageLimit.
modifyUsageLimit_amount :: Lens.Lens' ModifyUsageLimit (Core.Maybe Core.Integer)
modifyUsageLimit_amount = Lens.lens (\ModifyUsageLimit' {amount} -> amount) (\s@ModifyUsageLimit' {} a -> s {amount = a} :: ModifyUsageLimit)

-- | The new action that Amazon Redshift takes when the limit is reached. For
-- more information about this parameter, see UsageLimit.
modifyUsageLimit_breachAction :: Lens.Lens' ModifyUsageLimit (Core.Maybe UsageLimitBreachAction)
modifyUsageLimit_breachAction = Lens.lens (\ModifyUsageLimit' {breachAction} -> breachAction) (\s@ModifyUsageLimit' {} a -> s {breachAction = a} :: ModifyUsageLimit)

-- | The identifier of the usage limit to modify.
modifyUsageLimit_usageLimitId :: Lens.Lens' ModifyUsageLimit Core.Text
modifyUsageLimit_usageLimitId = Lens.lens (\ModifyUsageLimit' {usageLimitId} -> usageLimitId) (\s@ModifyUsageLimit' {} a -> s {usageLimitId = a} :: ModifyUsageLimit)

instance Core.AWSRequest ModifyUsageLimit where
  type AWSResponse ModifyUsageLimit = UsageLimit
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyUsageLimitResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable ModifyUsageLimit

instance Core.NFData ModifyUsageLimit

instance Core.ToHeaders ModifyUsageLimit where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyUsageLimit where
  toPath = Core.const "/"

instance Core.ToQuery ModifyUsageLimit where
  toQuery ModifyUsageLimit' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyUsageLimit" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Amount" Core.=: amount,
        "BreachAction" Core.=: breachAction,
        "UsageLimitId" Core.=: usageLimitId
      ]
