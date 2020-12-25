{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InferenceAcceleratorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InferenceAcceleratorInfo
  ( InferenceAcceleratorInfo (..),

    -- * Smart constructor
    mkInferenceAcceleratorInfo,

    -- * Lenses
    iaiAccelerators,
  )
where

import qualified Network.AWS.EC2.Types.InferenceDeviceInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'mkInferenceAcceleratorInfo' smart constructor.
newtype InferenceAcceleratorInfo = InferenceAcceleratorInfo'
  { -- | Describes the Inference accelerators for the instance type.
    accelerators :: Core.Maybe [Types.InferenceDeviceInfo]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InferenceAcceleratorInfo' value with any optional fields omitted.
mkInferenceAcceleratorInfo ::
  InferenceAcceleratorInfo
mkInferenceAcceleratorInfo =
  InferenceAcceleratorInfo' {accelerators = Core.Nothing}

-- | Describes the Inference accelerators for the instance type.
--
-- /Note:/ Consider using 'accelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaiAccelerators :: Lens.Lens' InferenceAcceleratorInfo (Core.Maybe [Types.InferenceDeviceInfo])
iaiAccelerators = Lens.field @"accelerators"
{-# DEPRECATED iaiAccelerators "Use generic-lens or generic-optics with 'accelerators' instead." #-}

instance Core.FromXML InferenceAcceleratorInfo where
  parseXML x =
    InferenceAcceleratorInfo'
      Core.<$> (x Core..@? "accelerators" Core..<@> Core.parseXMLList "member")
