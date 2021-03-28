{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FpgaImageState
  ( FpgaImageState (..)
  -- * Smart constructor
  , mkFpgaImageState
  -- * Lenses
  , fisCode
  , fisMessage
  ) where

import qualified Network.AWS.EC2.Types.FpgaImageStateCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of the bitstream generation process for an Amazon FPGA image (AFI).
--
-- /See:/ 'mkFpgaImageState' smart constructor.
data FpgaImageState = FpgaImageState'
  { code :: Core.Maybe Types.FpgaImageStateCode
    -- ^ The state. The following are the possible values:
--
--
--     * @pending@ - AFI bitstream generation is in progress.
--
--
--     * @available@ - The AFI is available for use.
--
--
--     * @failed@ - AFI bitstream generation failed.
--
--
--     * @unavailable@ - The AFI is no longer available for use.
--
--
  , message :: Core.Maybe Core.Text
    -- ^ If the state is @failed@ , this is the error message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FpgaImageState' value with any optional fields omitted.
mkFpgaImageState
    :: FpgaImageState
mkFpgaImageState
  = FpgaImageState'{code = Core.Nothing, message = Core.Nothing}

-- | The state. The following are the possible values:
--
--
--     * @pending@ - AFI bitstream generation is in progress.
--
--
--     * @available@ - The AFI is available for use.
--
--
--     * @failed@ - AFI bitstream generation failed.
--
--
--     * @unavailable@ - The AFI is no longer available for use.
--
--
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fisCode :: Lens.Lens' FpgaImageState (Core.Maybe Types.FpgaImageStateCode)
fisCode = Lens.field @"code"
{-# INLINEABLE fisCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | If the state is @failed@ , this is the error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fisMessage :: Lens.Lens' FpgaImageState (Core.Maybe Core.Text)
fisMessage = Lens.field @"message"
{-# INLINEABLE fisMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML FpgaImageState where
        parseXML x
          = FpgaImageState' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
