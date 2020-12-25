{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
  ( TestGridSessionArtifact (..),

    -- * Smart constructor
    mkTestGridSessionArtifact,

    -- * Lenses
    tgsaFilename,
    tgsaType,
    tgsaUrl,
  )
where

import qualified Network.AWS.DeviceFarm.Types.String as Types
import qualified Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Artifacts are video and other files that are produced in the process of running a browser in an automated context.
--
-- /See:/ 'mkTestGridSessionArtifact' smart constructor.
data TestGridSessionArtifact = TestGridSessionArtifact'
  { -- | The file name of the artifact.
    filename :: Core.Maybe Types.String,
    -- | The kind of artifact.
    type' :: Core.Maybe Types.TestGridSessionArtifactType,
    -- | A semi-stable URL to the content of the object.
    url :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TestGridSessionArtifact' value with any optional fields omitted.
mkTestGridSessionArtifact ::
  TestGridSessionArtifact
mkTestGridSessionArtifact =
  TestGridSessionArtifact'
    { filename = Core.Nothing,
      type' = Core.Nothing,
      url = Core.Nothing
    }

-- | The file name of the artifact.
--
-- /Note:/ Consider using 'filename' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaFilename :: Lens.Lens' TestGridSessionArtifact (Core.Maybe Types.String)
tgsaFilename = Lens.field @"filename"
{-# DEPRECATED tgsaFilename "Use generic-lens or generic-optics with 'filename' instead." #-}

-- | The kind of artifact.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaType :: Lens.Lens' TestGridSessionArtifact (Core.Maybe Types.TestGridSessionArtifactType)
tgsaType = Lens.field @"type'"
{-# DEPRECATED tgsaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A semi-stable URL to the content of the object.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaUrl :: Lens.Lens' TestGridSessionArtifact (Core.Maybe Types.String)
tgsaUrl = Lens.field @"url"
{-# DEPRECATED tgsaUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON TestGridSessionArtifact where
  parseJSON =
    Core.withObject "TestGridSessionArtifact" Core.$
      \x ->
        TestGridSessionArtifact'
          Core.<$> (x Core..:? "filename")
          Core.<*> (x Core..:? "type")
          Core.<*> (x Core..:? "url")
