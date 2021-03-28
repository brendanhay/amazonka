{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ElasticGpuSpecification
  ( ElasticGpuSpecification (..)
  -- * Smart constructor
  , mkElasticGpuSpecification
  -- * Lenses
  , egsType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A specification for an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpuSpecification' smart constructor.
newtype ElasticGpuSpecification = ElasticGpuSpecification'
  { type' :: Core.Text
    -- ^ The type of Elastic Graphics accelerator. For more information about the values to specify for @Type@ , see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html#elastic-graphics-basics Elastic Graphics Basics> , specifically the Elastic Graphics accelerator column, in the /Amazon Elastic Compute Cloud User Guide for Windows Instances/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticGpuSpecification' value with any optional fields omitted.
mkElasticGpuSpecification
    :: Core.Text -- ^ 'type\''
    -> ElasticGpuSpecification
mkElasticGpuSpecification type' = ElasticGpuSpecification'{type'}

-- | The type of Elastic Graphics accelerator. For more information about the values to specify for @Type@ , see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html#elastic-graphics-basics Elastic Graphics Basics> , specifically the Elastic Graphics accelerator column, in the /Amazon Elastic Compute Cloud User Guide for Windows Instances/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egsType :: Lens.Lens' ElasticGpuSpecification Core.Text
egsType = Lens.field @"type'"
{-# INLINEABLE egsType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery ElasticGpuSpecification where
        toQuery ElasticGpuSpecification{..} = Core.toQueryPair "Type" type'
