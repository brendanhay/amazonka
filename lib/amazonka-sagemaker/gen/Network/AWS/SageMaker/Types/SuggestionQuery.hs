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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.PropertyNameQuery

-- | Specified in the 'GetSearchSuggestions' request. Limits the property names that are included in the response.
--
-- /See:/ 'mkSuggestionQuery' smart constructor.
newtype SuggestionQuery = SuggestionQuery'
  { propertyNameQuery ::
      Lude.Maybe PropertyNameQuery
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuggestionQuery' with the minimum fields required to make a request.
--
-- * 'propertyNameQuery' - Defines a property name hint. Only property names that begin with the specified hint are included in the response.
mkSuggestionQuery ::
  SuggestionQuery
mkSuggestionQuery =
  SuggestionQuery' {propertyNameQuery = Lude.Nothing}

-- | Defines a property name hint. Only property names that begin with the specified hint are included in the response.
--
-- /Note:/ Consider using 'propertyNameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqPropertyNameQuery :: Lens.Lens' SuggestionQuery (Lude.Maybe PropertyNameQuery)
sqPropertyNameQuery = Lens.lens (propertyNameQuery :: SuggestionQuery -> Lude.Maybe PropertyNameQuery) (\s a -> s {propertyNameQuery = a} :: SuggestionQuery)
{-# DEPRECATED sqPropertyNameQuery "Use generic-lens or generic-optics with 'propertyNameQuery' instead." #-}

instance Lude.ToJSON SuggestionQuery where
  toJSON SuggestionQuery' {..} =
    Lude.object
      ( Lude.catMaybes
          [("PropertyNameQuery" Lude..=) Lude.<$> propertyNameQuery]
      )
