{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HoursOfOperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.HoursOfOperationSummary
  ( HoursOfOperationSummary (..)
  -- * Smart constructor
  , mkHoursOfOperationSummary
  -- * Lenses
  , hoosArn
  , hoosId
  , hoosName
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.HoursOfOperationId as Types
import qualified Network.AWS.Connect.Types.HoursOfOperationName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about hours of operation for a contact center.
--
-- /See:/ 'mkHoursOfOperationSummary' smart constructor.
data HoursOfOperationSummary = HoursOfOperationSummary'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the hours of operation.
  , id :: Core.Maybe Types.HoursOfOperationId
    -- ^ The identifier of the hours of operation.
  , name :: Core.Maybe Types.HoursOfOperationName
    -- ^ The name of the hours of operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HoursOfOperationSummary' value with any optional fields omitted.
mkHoursOfOperationSummary
    :: HoursOfOperationSummary
mkHoursOfOperationSummary
  = HoursOfOperationSummary'{arn = Core.Nothing, id = Core.Nothing,
                             name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the hours of operation.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoosArn :: Lens.Lens' HoursOfOperationSummary (Core.Maybe Types.ARN)
hoosArn = Lens.field @"arn"
{-# INLINEABLE hoosArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the hours of operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoosId :: Lens.Lens' HoursOfOperationSummary (Core.Maybe Types.HoursOfOperationId)
hoosId = Lens.field @"id"
{-# INLINEABLE hoosId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the hours of operation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoosName :: Lens.Lens' HoursOfOperationSummary (Core.Maybe Types.HoursOfOperationName)
hoosName = Lens.field @"name"
{-# INLINEABLE hoosName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON HoursOfOperationSummary where
        parseJSON
          = Core.withObject "HoursOfOperationSummary" Core.$
              \ x ->
                HoursOfOperationSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Name"
