{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AccountQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AccountQuota
  ( AccountQuota (..),

    -- * Smart constructor
    mkAccountQuota,

    -- * Lenses
    aqAccountQuotaName,
    aqMax,
    aqUsed,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a quota for an AWS account, for example, the number of replication instances allowed.
--
-- /See:/ 'mkAccountQuota' smart constructor.
data AccountQuota = AccountQuota'
  { -- | The name of the AWS DMS quota for this AWS account.
    accountQuotaName :: Core.Maybe Types.String,
    -- | The maximum allowed value for the quota.
    max :: Core.Maybe Core.Integer,
    -- | The amount currently used toward the quota maximum.
    used :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountQuota' value with any optional fields omitted.
mkAccountQuota ::
  AccountQuota
mkAccountQuota =
  AccountQuota'
    { accountQuotaName = Core.Nothing,
      max = Core.Nothing,
      used = Core.Nothing
    }

-- | The name of the AWS DMS quota for this AWS account.
--
-- /Note:/ Consider using 'accountQuotaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqAccountQuotaName :: Lens.Lens' AccountQuota (Core.Maybe Types.String)
aqAccountQuotaName = Lens.field @"accountQuotaName"
{-# DEPRECATED aqAccountQuotaName "Use generic-lens or generic-optics with 'accountQuotaName' instead." #-}

-- | The maximum allowed value for the quota.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqMax :: Lens.Lens' AccountQuota (Core.Maybe Core.Integer)
aqMax = Lens.field @"max"
{-# DEPRECATED aqMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The amount currently used toward the quota maximum.
--
-- /Note:/ Consider using 'used' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqUsed :: Lens.Lens' AccountQuota (Core.Maybe Core.Integer)
aqUsed = Lens.field @"used"
{-# DEPRECATED aqUsed "Use generic-lens or generic-optics with 'used' instead." #-}

instance Core.FromJSON AccountQuota where
  parseJSON =
    Core.withObject "AccountQuota" Core.$
      \x ->
        AccountQuota'
          Core.<$> (x Core..:? "AccountQuotaName")
          Core.<*> (x Core..:? "Max")
          Core.<*> (x Core..:? "Used")
