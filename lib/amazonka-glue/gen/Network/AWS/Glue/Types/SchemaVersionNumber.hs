{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionNumber
  ( SchemaVersionNumber (..),

    -- * Smart constructor
    mkSchemaVersionNumber,

    -- * Lenses
    svnLatestVersion,
    svnVersionNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkSchemaVersionNumber' smart constructor.
data SchemaVersionNumber = SchemaVersionNumber'
  { latestVersion :: Core.Maybe Core.Bool,
    versionNumber :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SchemaVersionNumber' value with any optional fields omitted.
mkSchemaVersionNumber ::
  SchemaVersionNumber
mkSchemaVersionNumber =
  SchemaVersionNumber'
    { latestVersion = Core.Nothing,
      versionNumber = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnLatestVersion :: Lens.Lens' SchemaVersionNumber (Core.Maybe Core.Bool)
svnLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED svnLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svnVersionNumber :: Lens.Lens' SchemaVersionNumber (Core.Maybe Core.Natural)
svnVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED svnVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance Core.FromJSON SchemaVersionNumber where
  toJSON SchemaVersionNumber {..} =
    Core.object
      ( Core.catMaybes
          [ ("LatestVersion" Core..=) Core.<$> latestVersion,
            ("VersionNumber" Core..=) Core.<$> versionNumber
          ]
      )
