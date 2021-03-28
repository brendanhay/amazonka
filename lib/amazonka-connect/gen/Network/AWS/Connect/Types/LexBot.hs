{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.LexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.LexBot
  ( LexBot (..)
  -- * Smart constructor
  , mkLexBot
  -- * Lenses
  , lbLexRegion
  , lbName
  ) where

import qualified Network.AWS.Connect.Types.LexRegion as Types
import qualified Network.AWS.Connect.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration information of an Amazon Lex bot.
--
-- /See:/ 'mkLexBot' smart constructor.
data LexBot = LexBot'
  { lexRegion :: Core.Maybe Types.LexRegion
    -- ^ The Region the Amazon Lex bot was created in.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the Amazon Lex bot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LexBot' value with any optional fields omitted.
mkLexBot
    :: LexBot
mkLexBot = LexBot'{lexRegion = Core.Nothing, name = Core.Nothing}

-- | The Region the Amazon Lex bot was created in.
--
-- /Note:/ Consider using 'lexRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLexRegion :: Lens.Lens' LexBot (Core.Maybe Types.LexRegion)
lbLexRegion = Lens.field @"lexRegion"
{-# INLINEABLE lbLexRegion #-}
{-# DEPRECATED lexRegion "Use generic-lens or generic-optics with 'lexRegion' instead"  #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbName :: Lens.Lens' LexBot (Core.Maybe Types.Name)
lbName = Lens.field @"name"
{-# INLINEABLE lbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON LexBot where
        toJSON LexBot{..}
          = Core.object
              (Core.catMaybes
                 [("LexRegion" Core..=) Core.<$> lexRegion,
                  ("Name" Core..=) Core.<$> name])

instance Core.FromJSON LexBot where
        parseJSON
          = Core.withObject "LexBot" Core.$
              \ x ->
                LexBot' Core.<$>
                  (x Core..:? "LexRegion") Core.<*> x Core..:? "Name"
