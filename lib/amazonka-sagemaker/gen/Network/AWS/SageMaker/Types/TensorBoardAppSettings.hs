-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TensorBoardAppSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TensorBoardAppSettings
  ( TensorBoardAppSettings (..),

    -- * Smart constructor
    mkTensorBoardAppSettings,

    -- * Lenses
    tbasDefaultResourceSpec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ResourceSpec

-- | The TensorBoard app settings.
--
-- /See:/ 'mkTensorBoardAppSettings' smart constructor.
newtype TensorBoardAppSettings = TensorBoardAppSettings'
  { defaultResourceSpec ::
      Lude.Maybe ResourceSpec
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TensorBoardAppSettings' with the minimum fields required to make a request.
--
-- * 'defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
mkTensorBoardAppSettings ::
  TensorBoardAppSettings
mkTensorBoardAppSettings =
  TensorBoardAppSettings' {defaultResourceSpec = Lude.Nothing}

-- | The default instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'defaultResourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbasDefaultResourceSpec :: Lens.Lens' TensorBoardAppSettings (Lude.Maybe ResourceSpec)
tbasDefaultResourceSpec = Lens.lens (defaultResourceSpec :: TensorBoardAppSettings -> Lude.Maybe ResourceSpec) (\s a -> s {defaultResourceSpec = a} :: TensorBoardAppSettings)
{-# DEPRECATED tbasDefaultResourceSpec "Use generic-lens or generic-optics with 'defaultResourceSpec' instead." #-}

instance Lude.FromJSON TensorBoardAppSettings where
  parseJSON =
    Lude.withObject
      "TensorBoardAppSettings"
      ( \x ->
          TensorBoardAppSettings'
            Lude.<$> (x Lude..:? "DefaultResourceSpec")
      )

instance Lude.ToJSON TensorBoardAppSettings where
  toJSON TensorBoardAppSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("DefaultResourceSpec" Lude..=) Lude.<$> defaultResourceSpec]
      )
