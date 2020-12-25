{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Resource
  ( Resource (..),

    -- * Smart constructor
    mkResource,

    -- * Lenses
    rId,
  )
where

import qualified Network.AWS.CodeStar.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a resource for a project.
--
-- /See:/ 'mkResource' smart constructor.
newtype Resource = Resource'
  { -- | The Amazon Resource Name (ARN) of the resource.
    id :: Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Resource' value with any optional fields omitted.
mkResource ::
  -- | 'id'
  Types.ResourceId ->
  Resource
mkResource id = Resource' {id}

-- | The Amazon Resource Name (ARN) of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Resource Types.ResourceId
rId = Lens.field @"id"
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject "Resource" Core.$
      \x -> Resource' Core.<$> (x Core..: "id")
