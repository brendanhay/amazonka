{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuSpecificationResponse
  ( ElasticGpuSpecificationResponse (..),

    -- * Smart constructor
    mkElasticGpuSpecificationResponse,

    -- * Lenses
    egsrType,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an elastic GPU.
--
-- /See:/ 'mkElasticGpuSpecificationResponse' smart constructor.
newtype ElasticGpuSpecificationResponse = ElasticGpuSpecificationResponse'
  { -- | The elastic GPU type.
    type' :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticGpuSpecificationResponse' value with any optional fields omitted.
mkElasticGpuSpecificationResponse ::
  ElasticGpuSpecificationResponse
mkElasticGpuSpecificationResponse =
  ElasticGpuSpecificationResponse' {type' = Core.Nothing}

-- | The elastic GPU type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
egsrType :: Lens.Lens' ElasticGpuSpecificationResponse (Core.Maybe Types.String)
egsrType = Lens.field @"type'"
{-# DEPRECATED egsrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromXML ElasticGpuSpecificationResponse where
  parseXML x =
    ElasticGpuSpecificationResponse' Core.<$> (x Core..@? "type")
