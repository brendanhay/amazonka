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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ResourcePropertyName as Types

-- | A property name returned from a @GetSearchSuggestions@ call that specifies a value in the @PropertyNameQuery@ field.
--
-- /See:/ 'mkPropertyNameSuggestion' smart constructor.
newtype PropertyNameSuggestion = PropertyNameSuggestion'
  { -- | A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
    propertyName :: Core.Maybe Types.ResourcePropertyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PropertyNameSuggestion' value with any optional fields omitted.
mkPropertyNameSuggestion ::
  PropertyNameSuggestion
mkPropertyNameSuggestion =
  PropertyNameSuggestion' {propertyName = Core.Nothing}

-- | A suggested property name based on what you entered in the search textbox in the Amazon SageMaker console.
--
-- /Note:/ Consider using 'propertyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPropertyName :: Lens.Lens' PropertyNameSuggestion (Core.Maybe Types.ResourcePropertyName)
pnsPropertyName = Lens.field @"propertyName"
{-# DEPRECATED pnsPropertyName "Use generic-lens or generic-optics with 'propertyName' instead." #-}

instance Core.FromJSON PropertyNameSuggestion where
  parseJSON =
    Core.withObject "PropertyNameSuggestion" Core.$
      \x -> PropertyNameSuggestion' Core.<$> (x Core..:? "PropertyName")
