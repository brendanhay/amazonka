{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.InputContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.InputContext
  ( InputContext (..)
  -- * Smart constructor
  , mkInputContext
  -- * Lenses
  , icName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.InputContextName as Types
import qualified Network.AWS.Prelude as Core

-- | The name of a context that must be active for an intent to be selected by Amazon Lex.
--
-- /See:/ 'mkInputContext' smart constructor.
newtype InputContext = InputContext'
  { name :: Types.InputContextName
    -- ^ The name of the context.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputContext' value with any optional fields omitted.
mkInputContext
    :: Types.InputContextName -- ^ 'name'
    -> InputContext
mkInputContext name = InputContext'{name}

-- | The name of the context.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icName :: Lens.Lens' InputContext Types.InputContextName
icName = Lens.field @"name"
{-# INLINEABLE icName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON InputContext where
        toJSON InputContext{..}
          = Core.object (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.FromJSON InputContext where
        parseJSON
          = Core.withObject "InputContext" Core.$
              \ x -> InputContext' Core.<$> (x Core..: "name")
