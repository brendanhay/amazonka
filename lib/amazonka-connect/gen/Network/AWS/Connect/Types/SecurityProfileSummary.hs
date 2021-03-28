{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.SecurityProfileSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.SecurityProfileSummary
  ( SecurityProfileSummary (..)
  -- * Smart constructor
  , mkSecurityProfileSummary
  -- * Lenses
  , spsArn
  , spsId
  , spsName
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.SecurityProfileId as Types
import qualified Network.AWS.Connect.Types.SecurityProfileName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a security profile.
--
-- /See:/ 'mkSecurityProfileSummary' smart constructor.
data SecurityProfileSummary = SecurityProfileSummary'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the security profile.
  , id :: Core.Maybe Types.SecurityProfileId
    -- ^ The identifier of the security profile.
  , name :: Core.Maybe Types.SecurityProfileName
    -- ^ The name of the security profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityProfileSummary' value with any optional fields omitted.
mkSecurityProfileSummary
    :: SecurityProfileSummary
mkSecurityProfileSummary
  = SecurityProfileSummary'{arn = Core.Nothing, id = Core.Nothing,
                            name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the security profile.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsArn :: Lens.Lens' SecurityProfileSummary (Core.Maybe Types.ARN)
spsArn = Lens.field @"arn"
{-# INLINEABLE spsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the security profile.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsId :: Lens.Lens' SecurityProfileSummary (Core.Maybe Types.SecurityProfileId)
spsId = Lens.field @"id"
{-# INLINEABLE spsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the security profile.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spsName :: Lens.Lens' SecurityProfileSummary (Core.Maybe Types.SecurityProfileName)
spsName = Lens.field @"name"
{-# INLINEABLE spsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON SecurityProfileSummary where
        parseJSON
          = Core.withObject "SecurityProfileSummary" Core.$
              \ x ->
                SecurityProfileSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Name"
