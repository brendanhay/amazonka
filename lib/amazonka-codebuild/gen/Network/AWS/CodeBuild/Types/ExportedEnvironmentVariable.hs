{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
  ( ExportedEnvironmentVariable (..)
  -- * Smart constructor
  , mkExportedEnvironmentVariable
  -- * Lenses
  , eevName
  , eevValue
  ) where

import qualified Network.AWS.CodeBuild.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an exported environment variable. 
--
-- /See:/ 'mkExportedEnvironmentVariable' smart constructor.
data ExportedEnvironmentVariable = ExportedEnvironmentVariable'
  { name :: Core.Maybe Types.Name
    -- ^ The name of this exported environment variable. 
  , value :: Core.Maybe Core.Text
    -- ^ The value assigned to this exported environment variable. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportedEnvironmentVariable' value with any optional fields omitted.
mkExportedEnvironmentVariable
    :: ExportedEnvironmentVariable
mkExportedEnvironmentVariable
  = ExportedEnvironmentVariable'{name = Core.Nothing,
                                 value = Core.Nothing}

-- | The name of this exported environment variable. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eevName :: Lens.Lens' ExportedEnvironmentVariable (Core.Maybe Types.Name)
eevName = Lens.field @"name"
{-# INLINEABLE eevName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value assigned to this exported environment variable. 
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eevValue :: Lens.Lens' ExportedEnvironmentVariable (Core.Maybe Core.Text)
eevValue = Lens.field @"value"
{-# INLINEABLE eevValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON ExportedEnvironmentVariable where
        parseJSON
          = Core.withObject "ExportedEnvironmentVariable" Core.$
              \ x ->
                ExportedEnvironmentVariable' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "value"
