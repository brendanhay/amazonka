{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InferenceDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InferenceDeviceInfo
  ( InferenceDeviceInfo (..)
  -- * Smart constructor
  , mkInferenceDeviceInfo
  -- * Lenses
  , idiCount
  , idiManufacturer
  , idiName
  ) where

import qualified Network.AWS.EC2.Types.InferenceDeviceManufacturerName as Types
import qualified Network.AWS.EC2.Types.InferenceDeviceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'mkInferenceDeviceInfo' smart constructor.
data InferenceDeviceInfo = InferenceDeviceInfo'
  { count :: Core.Maybe Core.Int
    -- ^ The number of Inference accelerators for the instance type.
  , manufacturer :: Core.Maybe Types.InferenceDeviceManufacturerName
    -- ^ The manufacturer of the Inference accelerator.
  , name :: Core.Maybe Types.InferenceDeviceName
    -- ^ The name of the Inference accelerator.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InferenceDeviceInfo' value with any optional fields omitted.
mkInferenceDeviceInfo
    :: InferenceDeviceInfo
mkInferenceDeviceInfo
  = InferenceDeviceInfo'{count = Core.Nothing,
                         manufacturer = Core.Nothing, name = Core.Nothing}

-- | The number of Inference accelerators for the instance type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiCount :: Lens.Lens' InferenceDeviceInfo (Core.Maybe Core.Int)
idiCount = Lens.field @"count"
{-# INLINEABLE idiCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The manufacturer of the Inference accelerator.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiManufacturer :: Lens.Lens' InferenceDeviceInfo (Core.Maybe Types.InferenceDeviceManufacturerName)
idiManufacturer = Lens.field @"manufacturer"
{-# INLINEABLE idiManufacturer #-}
{-# DEPRECATED manufacturer "Use generic-lens or generic-optics with 'manufacturer' instead"  #-}

-- | The name of the Inference accelerator.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiName :: Lens.Lens' InferenceDeviceInfo (Core.Maybe Types.InferenceDeviceName)
idiName = Lens.field @"name"
{-# INLINEABLE idiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML InferenceDeviceInfo where
        parseXML x
          = InferenceDeviceInfo' Core.<$>
              (x Core..@? "count") Core.<*> x Core..@? "manufacturer" Core.<*>
                x Core..@? "name"
