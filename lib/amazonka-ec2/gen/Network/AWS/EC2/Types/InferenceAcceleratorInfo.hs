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

import Network.AWS.EC2.Types.InferenceDeviceInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'mkInferenceAcceleratorInfo' smart constructor.
newtype InferenceAcceleratorInfo = InferenceAcceleratorInfo'
  { accelerators ::
      Lude.Maybe [InferenceDeviceInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InferenceAcceleratorInfo' with the minimum fields required to make a request.
--
-- * 'accelerators' - Describes the Inference accelerators for the instance type.
mkInferenceAcceleratorInfo ::
  InferenceAcceleratorInfo
mkInferenceAcceleratorInfo =
  InferenceAcceleratorInfo' {accelerators = Lude.Nothing}

-- | Describes the Inference accelerators for the instance type.
--
-- /Note:/ Consider using 'accelerators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaiAccelerators :: Lens.Lens' InferenceAcceleratorInfo (Lude.Maybe [InferenceDeviceInfo])
iaiAccelerators = Lens.lens (accelerators :: InferenceAcceleratorInfo -> Lude.Maybe [InferenceDeviceInfo]) (\s a -> s {accelerators = a} :: InferenceAcceleratorInfo)
{-# DEPRECATED iaiAccelerators "Use generic-lens or generic-optics with 'accelerators' instead." #-}

instance Lude.FromXML InferenceAcceleratorInfo where
  parseXML x =
    InferenceAcceleratorInfo'
      Lude.<$> ( x Lude..@? "accelerators" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
