-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageState
  ( FpgaImageState (..),

    -- * Smart constructor
    mkFpgaImageState,

    -- * Lenses
    fisCode,
    fisMessage,
  )
where

import Network.AWS.EC2.Types.FpgaImageStateCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the state of the bitstream generation process for an Amazon FPGA image (AFI).
--
-- /See:/ 'mkFpgaImageState' smart constructor.
data FpgaImageState = FpgaImageState'
  { code ::
      Lude.Maybe FpgaImageStateCode,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FpgaImageState' with the minimum fields required to make a request.
--
-- * 'code' - The state. The following are the possible values:
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
-- * 'message' - If the state is @failed@ , this is the error message.
mkFpgaImageState ::
  FpgaImageState
mkFpgaImageState =
  FpgaImageState' {code = Lude.Nothing, message = Lude.Nothing}

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
fisCode :: Lens.Lens' FpgaImageState (Lude.Maybe FpgaImageStateCode)
fisCode = Lens.lens (code :: FpgaImageState -> Lude.Maybe FpgaImageStateCode) (\s a -> s {code = a} :: FpgaImageState)
{-# DEPRECATED fisCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | If the state is @failed@ , this is the error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fisMessage :: Lens.Lens' FpgaImageState (Lude.Maybe Lude.Text)
fisMessage = Lens.lens (message :: FpgaImageState -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: FpgaImageState)
{-# DEPRECATED fisMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML FpgaImageState where
  parseXML x =
    FpgaImageState'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
