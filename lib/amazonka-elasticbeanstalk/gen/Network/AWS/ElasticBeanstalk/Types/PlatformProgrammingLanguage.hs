{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.PlatformProgrammingLanguage
  ( PlatformProgrammingLanguage (..)
  -- * Smart constructor
  , mkPlatformProgrammingLanguage
  -- * Lenses
  , pplName
  , pplVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A programming language supported by the platform.
--
-- /See:/ 'mkPlatformProgrammingLanguage' smart constructor.
data PlatformProgrammingLanguage = PlatformProgrammingLanguage'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the programming language.
  , version :: Core.Maybe Core.Text
    -- ^ The version of the programming language.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformProgrammingLanguage' value with any optional fields omitted.
mkPlatformProgrammingLanguage
    :: PlatformProgrammingLanguage
mkPlatformProgrammingLanguage
  = PlatformProgrammingLanguage'{name = Core.Nothing,
                                 version = Core.Nothing}

-- | The name of the programming language.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplName :: Lens.Lens' PlatformProgrammingLanguage (Core.Maybe Core.Text)
pplName = Lens.field @"name"
{-# INLINEABLE pplName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the programming language.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplVersion :: Lens.Lens' PlatformProgrammingLanguage (Core.Maybe Core.Text)
pplVersion = Lens.field @"version"
{-# INLINEABLE pplVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromXML PlatformProgrammingLanguage where
        parseXML x
          = PlatformProgrammingLanguage' Core.<$>
              (x Core..@? "Name") Core.<*> x Core..@? "Version"
