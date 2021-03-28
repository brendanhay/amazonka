{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceByResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ComplianceByResource
  ( ComplianceByResource (..)
  -- * Smart constructor
  , mkComplianceByResource
  -- * Lenses
  , cbrCompliance
  , cbrResourceId
  , cbrResourceType
  ) where

import qualified Network.AWS.Config.Types.BaseResourceId as Types
import qualified Network.AWS.Config.Types.Compliance as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Indicates whether an AWS resource that is evaluated according to one or more AWS Config rules is compliant. A resource is compliant if it complies with all of the rules that evaluate it. A resource is noncompliant if it does not comply with one or more of these rules.
--
-- /See:/ 'mkComplianceByResource' smart constructor.
data ComplianceByResource = ComplianceByResource'
  { compliance :: Core.Maybe Types.Compliance
    -- ^ Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
  , resourceId :: Core.Maybe Types.BaseResourceId
    -- ^ The ID of the AWS resource that was evaluated.
  , resourceType :: Core.Maybe Types.StringWithCharLimit256
    -- ^ The type of the AWS resource that was evaluated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceByResource' value with any optional fields omitted.
mkComplianceByResource
    :: ComplianceByResource
mkComplianceByResource
  = ComplianceByResource'{compliance = Core.Nothing,
                          resourceId = Core.Nothing, resourceType = Core.Nothing}

-- | Indicates whether the AWS resource complies with all of the AWS Config rules that evaluated it.
--
-- /Note:/ Consider using 'compliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrCompliance :: Lens.Lens' ComplianceByResource (Core.Maybe Types.Compliance)
cbrCompliance = Lens.field @"compliance"
{-# INLINEABLE cbrCompliance #-}
{-# DEPRECATED compliance "Use generic-lens or generic-optics with 'compliance' instead"  #-}

-- | The ID of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrResourceId :: Lens.Lens' ComplianceByResource (Core.Maybe Types.BaseResourceId)
cbrResourceId = Lens.field @"resourceId"
{-# INLINEABLE cbrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of the AWS resource that was evaluated.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrResourceType :: Lens.Lens' ComplianceByResource (Core.Maybe Types.StringWithCharLimit256)
cbrResourceType = Lens.field @"resourceType"
{-# INLINEABLE cbrResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON ComplianceByResource where
        parseJSON
          = Core.withObject "ComplianceByResource" Core.$
              \ x ->
                ComplianceByResource' Core.<$>
                  (x Core..:? "Compliance") Core.<*> x Core..:? "ResourceId" Core.<*>
                    x Core..:? "ResourceType"
