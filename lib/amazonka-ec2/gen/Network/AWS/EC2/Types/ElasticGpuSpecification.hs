{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuSpecification
  ( ElasticGpuSpecification (..),

    -- * Smart constructor
    mkElasticGpuSpecification,

    -- * Lenses
    egsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A specification for an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpuSpecification' smart constructor.
newtype ElasticGpuSpecification = ElasticGpuSpecification'
  { type' ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElasticGpuSpecification' with the minimum fields required to make a request.
--
-- * 'type'' - The type of Elastic Graphics accelerator. For more information about the values to specify for @Type@ , see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html#elastic-graphics-basics Elastic Graphics Basics> , specifically the Elastic Graphics accelerator column, in the /Amazon Elastic Compute Cloud User Guide for Windows Instances/ .
mkElasticGpuSpecification ::
  -- | 'type''
  Lude.Text ->
  ElasticGpuSpecification
mkElasticGpuSpecification pType_ =
  ElasticGpuSpecification' {type' = pType_}

-- | The type of Elastic Graphics accelerator. For more information about the values to specify for @Type@ , see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html#elastic-graphics-basics Elastic Graphics Basics> , specifically the Elastic Graphics accelerator column, in the /Amazon Elastic Compute Cloud User Guide for Windows Instances/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egsType :: Lens.Lens' ElasticGpuSpecification Lude.Text
egsType = Lens.lens (type' :: ElasticGpuSpecification -> Lude.Text) (\s a -> s {type' = a} :: ElasticGpuSpecification)
{-# DEPRECATED egsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToQuery ElasticGpuSpecification where
  toQuery ElasticGpuSpecification' {..} =
    Lude.mconcat ["Type" Lude.=: type']
