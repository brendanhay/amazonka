{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.DataRetrievalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.DataRetrievalPolicy
  ( DataRetrievalPolicy (..)
  -- * Smart constructor
  , mkDataRetrievalPolicy
  -- * Lenses
  , drpRules
  ) where

import qualified Network.AWS.Glacier.Types.DataRetrievalRule as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Data retrieval policy.
--
-- /See:/ 'mkDataRetrievalPolicy' smart constructor.
newtype DataRetrievalPolicy = DataRetrievalPolicy'
  { rules :: Core.Maybe [Types.DataRetrievalRule]
    -- ^ The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DataRetrievalPolicy' value with any optional fields omitted.
mkDataRetrievalPolicy
    :: DataRetrievalPolicy
mkDataRetrievalPolicy = DataRetrievalPolicy'{rules = Core.Nothing}

-- | The policy rule. Although this is a list type, currently there must be only one rule, which contains a Strategy field and optionally a BytesPerHour field.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRules :: Lens.Lens' DataRetrievalPolicy (Core.Maybe [Types.DataRetrievalRule])
drpRules = Lens.field @"rules"
{-# INLINEABLE drpRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

instance Core.FromJSON DataRetrievalPolicy where
        toJSON DataRetrievalPolicy{..}
          = Core.object (Core.catMaybes [("Rules" Core..=) Core.<$> rules])

instance Core.FromJSON DataRetrievalPolicy where
        parseJSON
          = Core.withObject "DataRetrievalPolicy" Core.$
              \ x -> DataRetrievalPolicy' Core.<$> (x Core..:? "Rules")
