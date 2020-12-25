{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.PlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.PlatformApplication
  ( PlatformApplication (..),

    -- * Smart constructor
    mkPlatformApplication,

    -- * Lenses
    paAttributes,
    paPlatformApplicationArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SNS.Types.String as Types

-- | Platform application object.
--
-- /See:/ 'mkPlatformApplication' smart constructor.
data PlatformApplication = PlatformApplication'
  { -- | Attributes for platform application object.
    attributes :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | PlatformApplicationArn for platform application object.
    platformApplicationArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlatformApplication' value with any optional fields omitted.
mkPlatformApplication ::
  PlatformApplication
mkPlatformApplication =
  PlatformApplication'
    { attributes = Core.Nothing,
      platformApplicationArn = Core.Nothing
    }

-- | Attributes for platform application object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributes :: Lens.Lens' PlatformApplication (Core.Maybe (Core.HashMap Types.String Types.String))
paAttributes = Lens.field @"attributes"
{-# DEPRECATED paAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | PlatformApplicationArn for platform application object.
--
-- /Note:/ Consider using 'platformApplicationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPlatformApplicationArn :: Lens.Lens' PlatformApplication (Core.Maybe Types.String)
paPlatformApplicationArn = Lens.field @"platformApplicationArn"
{-# DEPRECATED paPlatformApplicationArn "Use generic-lens or generic-optics with 'platformApplicationArn' instead." #-}

instance Core.FromXML PlatformApplication where
  parseXML x =
    PlatformApplication'
      Core.<$> ( x Core..@? "Attributes"
                   Core..<@> Core.parseXMLMap "entry" "key" "value"
               )
      Core.<*> (x Core..@? "PlatformApplicationArn")
