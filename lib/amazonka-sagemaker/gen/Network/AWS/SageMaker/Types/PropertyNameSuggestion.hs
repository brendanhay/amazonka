{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PropertyNameSuggestion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PropertyNameSuggestion
  ( PropertyNameSuggestion (..),

    -- * Smart constructor
    mkPropertyNameSuggestion,

    -- * Lenses
    pnsPropertyName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A property name returned from a @GetSearchSuggestions@ call that specifies a value in the @PropertyNameQuery@ field.
--
-- /See:/ 'mkPropertyNameSuggestion' smart constructor.
newtype PropertyNameSuggestion = PropertyNameSuggestion'
  { -- | A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
    propertyName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PropertyNameSuggestion' with the minimum fields required to make a request.
--
-- * 'propertyName' - A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
mkPropertyNameSuggestion ::
  PropertyNameSuggestion
mkPropertyNameSuggestion =
  PropertyNameSuggestion' {propertyName = Lude.Nothing}

-- | A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
--
-- /Note:/ Consider using 'propertyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPropertyName :: Lens.Lens' PropertyNameSuggestion (Lude.Maybe Lude.Text)
pnsPropertyName = Lens.lens (propertyName :: PropertyNameSuggestion -> Lude.Maybe Lude.Text) (\s a -> s {propertyName = a} :: PropertyNameSuggestion)
{-# DEPRECATED pnsPropertyName "Use generic-lens or generic-optics with 'propertyName' instead." #-}

instance Lude.FromJSON PropertyNameSuggestion where
  parseJSON =
    Lude.withObject
      "PropertyNameSuggestion"
      ( \x ->
          PropertyNameSuggestion' Lude.<$> (x Lude..:? "PropertyName")
      )
