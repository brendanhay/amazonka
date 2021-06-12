{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PropertyNameSuggestion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PropertyNameSuggestion where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A property name returned from a @GetSearchSuggestions@ call that
-- specifies a value in the @PropertyNameQuery@ field.
--
-- /See:/ 'newPropertyNameSuggestion' smart constructor.
data PropertyNameSuggestion = PropertyNameSuggestion'
  { -- | A suggested property name based on what you entered in the search
    -- textbox in the Amazon SageMaker console.
    propertyName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PropertyNameSuggestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyName', 'propertyNameSuggestion_propertyName' - A suggested property name based on what you entered in the search
-- textbox in the Amazon SageMaker console.
newPropertyNameSuggestion ::
  PropertyNameSuggestion
newPropertyNameSuggestion =
  PropertyNameSuggestion'
    { propertyName =
        Core.Nothing
    }

-- | A suggested property name based on what you entered in the search
-- textbox in the Amazon SageMaker console.
propertyNameSuggestion_propertyName :: Lens.Lens' PropertyNameSuggestion (Core.Maybe Core.Text)
propertyNameSuggestion_propertyName = Lens.lens (\PropertyNameSuggestion' {propertyName} -> propertyName) (\s@PropertyNameSuggestion' {} a -> s {propertyName = a} :: PropertyNameSuggestion)

instance Core.FromJSON PropertyNameSuggestion where
  parseJSON =
    Core.withObject
      "PropertyNameSuggestion"
      ( \x ->
          PropertyNameSuggestion'
            Core.<$> (x Core..:? "PropertyName")
      )

instance Core.Hashable PropertyNameSuggestion

instance Core.NFData PropertyNameSuggestion
