{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SuggestionQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SuggestionQuery
  ( SuggestionQuery (..),

    -- * Smart constructor
    mkSuggestionQuery,

    -- * Lenses
    sqPropertyNameQuery,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.PropertyNameQuery as Types

-- | Specified in the 'GetSearchSuggestions' request. Limits the property names that are included in the response.
--
-- /See:/ 'mkSuggestionQuery' smart constructor.
newtype SuggestionQuery = SuggestionQuery'
  { -- | Defines a property name hint. Only property names that begin with the specified hint are included in the response.
    propertyNameQuery :: Core.Maybe Types.PropertyNameQuery
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SuggestionQuery' value with any optional fields omitted.
mkSuggestionQuery ::
  SuggestionQuery
mkSuggestionQuery =
  SuggestionQuery' {propertyNameQuery = Core.Nothing}

-- | Defines a property name hint. Only property names that begin with the specified hint are included in the response.
--
-- /Note:/ Consider using 'propertyNameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqPropertyNameQuery :: Lens.Lens' SuggestionQuery (Core.Maybe Types.PropertyNameQuery)
sqPropertyNameQuery = Lens.field @"propertyNameQuery"
{-# DEPRECATED sqPropertyNameQuery "Use generic-lens or generic-optics with 'propertyNameQuery' instead." #-}

instance Core.FromJSON SuggestionQuery where
  toJSON SuggestionQuery {..} =
    Core.object
      ( Core.catMaybes
          [("PropertyNameQuery" Core..=) Core.<$> propertyNameQuery]
      )
