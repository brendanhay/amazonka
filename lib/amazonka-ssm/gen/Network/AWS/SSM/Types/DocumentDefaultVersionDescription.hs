{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentDefaultVersionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentDefaultVersionDescription
  ( DocumentDefaultVersionDescription (..),

    -- * Smart constructor
    mkDocumentDefaultVersionDescription,

    -- * Lenses
    ddvdDefaultVersion,
    ddvdDefaultVersionName,
    ddvdName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DocumentName as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.DocumentVersionName as Types

-- | A default version of a document.
--
-- /See:/ 'mkDocumentDefaultVersionDescription' smart constructor.
data DocumentDefaultVersionDescription = DocumentDefaultVersionDescription'
  { -- | The default version of the document.
    defaultVersion :: Core.Maybe Types.DocumentVersion,
    -- | The default version of the artifact associated with the document.
    defaultVersionName :: Core.Maybe Types.DocumentVersionName,
    -- | The name of the document.
    name :: Core.Maybe Types.DocumentName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentDefaultVersionDescription' value with any optional fields omitted.
mkDocumentDefaultVersionDescription ::
  DocumentDefaultVersionDescription
mkDocumentDefaultVersionDescription =
  DocumentDefaultVersionDescription'
    { defaultVersion = Core.Nothing,
      defaultVersionName = Core.Nothing,
      name = Core.Nothing
    }

-- | The default version of the document.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvdDefaultVersion :: Lens.Lens' DocumentDefaultVersionDescription (Core.Maybe Types.DocumentVersion)
ddvdDefaultVersion = Lens.field @"defaultVersion"
{-# DEPRECATED ddvdDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | The default version of the artifact associated with the document.
--
-- /Note:/ Consider using 'defaultVersionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvdDefaultVersionName :: Lens.Lens' DocumentDefaultVersionDescription (Core.Maybe Types.DocumentVersionName)
ddvdDefaultVersionName = Lens.field @"defaultVersionName"
{-# DEPRECATED ddvdDefaultVersionName "Use generic-lens or generic-optics with 'defaultVersionName' instead." #-}

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddvdName :: Lens.Lens' DocumentDefaultVersionDescription (Core.Maybe Types.DocumentName)
ddvdName = Lens.field @"name"
{-# DEPRECATED ddvdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DocumentDefaultVersionDescription where
  parseJSON =
    Core.withObject "DocumentDefaultVersionDescription" Core.$
      \x ->
        DocumentDefaultVersionDescription'
          Core.<$> (x Core..:? "DefaultVersion")
          Core.<*> (x Core..:? "DefaultVersionName")
          Core.<*> (x Core..:? "Name")
