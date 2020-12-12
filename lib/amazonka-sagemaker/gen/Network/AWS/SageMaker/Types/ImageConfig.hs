{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageConfig
  ( ImageConfig (..),

    -- * Smart constructor
    mkImageConfig,

    -- * Lenses
    icRepositoryAccessMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.RepositoryAccessMode

-- | Specifies whether the model container is in Amazon ECR or a private Docker registry accessible from your Amazon Virtual Private Cloud (VPC).
--
-- /See:/ 'mkImageConfig' smart constructor.
newtype ImageConfig = ImageConfig'
  { repositoryAccessMode ::
      RepositoryAccessMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageConfig' with the minimum fields required to make a request.
--
-- * 'repositoryAccessMode' - Set this to one of the following values:
--
--
--     * @Platform@ - The model image is hosted in Amazon ECR.
--
--
--     * @Vpc@ - The model image is hosted in a private Docker registry in your VPC.
mkImageConfig ::
  -- | 'repositoryAccessMode'
  RepositoryAccessMode ->
  ImageConfig
mkImageConfig pRepositoryAccessMode_ =
  ImageConfig' {repositoryAccessMode = pRepositoryAccessMode_}

-- | Set this to one of the following values:
--
--
--     * @Platform@ - The model image is hosted in Amazon ECR.
--
--
--     * @Vpc@ - The model image is hosted in a private Docker registry in your VPC.
--
--
--
-- /Note:/ Consider using 'repositoryAccessMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icRepositoryAccessMode :: Lens.Lens' ImageConfig RepositoryAccessMode
icRepositoryAccessMode = Lens.lens (repositoryAccessMode :: ImageConfig -> RepositoryAccessMode) (\s a -> s {repositoryAccessMode = a} :: ImageConfig)
{-# DEPRECATED icRepositoryAccessMode "Use generic-lens or generic-optics with 'repositoryAccessMode' instead." #-}

instance Lude.FromJSON ImageConfig where
  parseJSON =
    Lude.withObject
      "ImageConfig"
      (\x -> ImageConfig' Lude.<$> (x Lude..: "RepositoryAccessMode"))

instance Lude.ToJSON ImageConfig where
  toJSON ImageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RepositoryAccessMode" Lude..= repositoryAccessMode)]
      )
