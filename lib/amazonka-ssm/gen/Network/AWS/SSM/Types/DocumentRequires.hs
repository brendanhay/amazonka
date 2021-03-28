{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentRequires
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentRequires
  ( DocumentRequires (..)
  -- * Smart constructor
  , mkDocumentRequires
  -- * Lenses
  , drName
  , drVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DocumentARN as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types

-- | An SSM document required by the current document.
--
-- /See:/ 'mkDocumentRequires' smart constructor.
data DocumentRequires = DocumentRequires'
  { name :: Types.DocumentARN
    -- ^ The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
  , version :: Core.Maybe Types.DocumentVersion
    -- ^ The document version required by the current document.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentRequires' value with any optional fields omitted.
mkDocumentRequires
    :: Types.DocumentARN -- ^ 'name'
    -> DocumentRequires
mkDocumentRequires name
  = DocumentRequires'{name, version = Core.Nothing}

-- | The name of the required SSM document. The name can be an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drName :: Lens.Lens' DocumentRequires Types.DocumentARN
drName = Lens.field @"name"
{-# INLINEABLE drName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The document version required by the current document.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drVersion :: Lens.Lens' DocumentRequires (Core.Maybe Types.DocumentVersion)
drVersion = Lens.field @"version"
{-# INLINEABLE drVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON DocumentRequires where
        toJSON DocumentRequires{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("Version" Core..=) Core.<$> version])

instance Core.FromJSON DocumentRequires where
        parseJSON
          = Core.withObject "DocumentRequires" Core.$
              \ x ->
                DocumentRequires' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..:? "Version"
