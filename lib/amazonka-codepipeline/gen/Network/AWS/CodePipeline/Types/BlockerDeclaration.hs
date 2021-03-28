{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.BlockerDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.BlockerDeclaration
  ( BlockerDeclaration (..)
  -- * Smart constructor
  , mkBlockerDeclaration
  -- * Lenses
  , bdName
  , bdType
  ) where

import qualified Network.AWS.CodePipeline.Types.BlockerType as Types
import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Reserved for future use.
--
-- /See:/ 'mkBlockerDeclaration' smart constructor.
data BlockerDeclaration = BlockerDeclaration'
  { name :: Types.Name
    -- ^ Reserved for future use.
  , type' :: Types.BlockerType
    -- ^ Reserved for future use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlockerDeclaration' value with any optional fields omitted.
mkBlockerDeclaration
    :: Types.Name -- ^ 'name'
    -> Types.BlockerType -- ^ 'type\''
    -> BlockerDeclaration
mkBlockerDeclaration name type' = BlockerDeclaration'{name, type'}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdName :: Lens.Lens' BlockerDeclaration Types.Name
bdName = Lens.field @"name"
{-# INLINEABLE bdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdType :: Lens.Lens' BlockerDeclaration Types.BlockerType
bdType = Lens.field @"type'"
{-# INLINEABLE bdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON BlockerDeclaration where
        toJSON BlockerDeclaration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("type" Core..= type')])

instance Core.FromJSON BlockerDeclaration where
        parseJSON
          = Core.withObject "BlockerDeclaration" Core.$
              \ x ->
                BlockerDeclaration' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "type"
