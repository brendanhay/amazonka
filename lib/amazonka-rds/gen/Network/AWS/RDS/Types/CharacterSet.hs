{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.CharacterSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.CharacterSet
  ( CharacterSet (..)
  -- * Smart constructor
  , mkCharacterSet
  -- * Lenses
  , csCharacterSetDescription
  , csCharacterSetName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used as a response element in the action @DescribeDBEngineVersions@ . 
--
-- /See:/ 'mkCharacterSet' smart constructor.
data CharacterSet = CharacterSet'
  { characterSetDescription :: Core.Maybe Core.Text
    -- ^ The description of the character set.
  , characterSetName :: Core.Maybe Core.Text
    -- ^ The name of the character set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CharacterSet' value with any optional fields omitted.
mkCharacterSet
    :: CharacterSet
mkCharacterSet
  = CharacterSet'{characterSetDescription = Core.Nothing,
                  characterSetName = Core.Nothing}

-- | The description of the character set.
--
-- /Note:/ Consider using 'characterSetDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCharacterSetDescription :: Lens.Lens' CharacterSet (Core.Maybe Core.Text)
csCharacterSetDescription = Lens.field @"characterSetDescription"
{-# INLINEABLE csCharacterSetDescription #-}
{-# DEPRECATED characterSetDescription "Use generic-lens or generic-optics with 'characterSetDescription' instead"  #-}

-- | The name of the character set.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCharacterSetName :: Lens.Lens' CharacterSet (Core.Maybe Core.Text)
csCharacterSetName = Lens.field @"characterSetName"
{-# INLINEABLE csCharacterSetName #-}
{-# DEPRECATED characterSetName "Use generic-lens or generic-optics with 'characterSetName' instead"  #-}

instance Core.FromXML CharacterSet where
        parseXML x
          = CharacterSet' Core.<$>
              (x Core..@? "CharacterSetDescription") Core.<*>
                x Core..@? "CharacterSetName"
