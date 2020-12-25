{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
  ( PlatformProgrammingLanguage (..),

    -- * Smart constructor
    mkPlatformProgrammingLanguage,

    -- * Lenses
    pplName,
    pplVersion,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.Name as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Version as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A programming language supported by the platform.
--
-- /See:/ 'mkPlatformProgrammingLanguage' smart constructor.
data PlatformProgrammingLanguage = PlatformProgrammingLanguage'
  { -- | The name of the programming language.
    name :: Core.Maybe Types.Name,
    -- | The version of the programming language.
    version :: Core.Maybe Types.Version
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformProgrammingLanguage' value with any optional fields omitted.
mkPlatformProgrammingLanguage ::
  PlatformProgrammingLanguage
mkPlatformProgrammingLanguage =
  PlatformProgrammingLanguage'
    { name = Core.Nothing,
      version = Core.Nothing
    }

-- | The name of the programming language.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplName :: Lens.Lens' PlatformProgrammingLanguage (Core.Maybe Types.Name)
pplName = Lens.field @"name"
{-# DEPRECATED pplName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the programming language.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplVersion :: Lens.Lens' PlatformProgrammingLanguage (Core.Maybe Types.Version)
pplVersion = Lens.field @"version"
{-# DEPRECATED pplVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromXML PlatformProgrammingLanguage where
  parseXML x =
    PlatformProgrammingLanguage'
      Core.<$> (x Core..@? "Name") Core.<*> (x Core..@? "Version")
