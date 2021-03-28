{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PropertyNameQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.PropertyNameQuery
  ( PropertyNameQuery (..)
  -- * Smart constructor
  , mkPropertyNameQuery
  -- * Lenses
  , pnqPropertyNameHint
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.PropertyNameHint as Types

-- | Part of the @SuggestionQuery@ type. Specifies a hint for retrieving property names that begin with the specified text.
--
-- /See:/ 'mkPropertyNameQuery' smart constructor.
newtype PropertyNameQuery = PropertyNameQuery'
  { propertyNameHint :: Types.PropertyNameHint
    -- ^ Text that begins a property's name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PropertyNameQuery' value with any optional fields omitted.
mkPropertyNameQuery
    :: Types.PropertyNameHint -- ^ 'propertyNameHint'
    -> PropertyNameQuery
mkPropertyNameQuery propertyNameHint
  = PropertyNameQuery'{propertyNameHint}

-- | Text that begins a property's name.
--
-- /Note:/ Consider using 'propertyNameHint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnqPropertyNameHint :: Lens.Lens' PropertyNameQuery Types.PropertyNameHint
pnqPropertyNameHint = Lens.field @"propertyNameHint"
{-# INLINEABLE pnqPropertyNameHint #-}
{-# DEPRECATED propertyNameHint "Use generic-lens or generic-optics with 'propertyNameHint' instead"  #-}

instance Core.FromJSON PropertyNameQuery where
        toJSON PropertyNameQuery{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("PropertyNameHint" Core..= propertyNameHint)])
