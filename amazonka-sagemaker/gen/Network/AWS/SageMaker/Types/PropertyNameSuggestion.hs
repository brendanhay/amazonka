{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A property name returned from a @GetSearchSuggestions@ call that
-- specifies a value in the @PropertyNameQuery@ field.
--
-- /See:/ 'newPropertyNameSuggestion' smart constructor.
data PropertyNameSuggestion = PropertyNameSuggestion'
  { -- | A suggested property name based on what you entered in the search
    -- textbox in the Amazon SageMaker console.
    propertyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | A suggested property name based on what you entered in the search
-- textbox in the Amazon SageMaker console.
propertyNameSuggestion_propertyName :: Lens.Lens' PropertyNameSuggestion (Prelude.Maybe Prelude.Text)
propertyNameSuggestion_propertyName = Lens.lens (\PropertyNameSuggestion' {propertyName} -> propertyName) (\s@PropertyNameSuggestion' {} a -> s {propertyName = a} :: PropertyNameSuggestion)

instance Prelude.FromJSON PropertyNameSuggestion where
  parseJSON =
    Prelude.withObject
      "PropertyNameSuggestion"
      ( \x ->
          PropertyNameSuggestion'
            Prelude.<$> (x Prelude..:? "PropertyName")
      )

instance Prelude.Hashable PropertyNameSuggestion

instance Prelude.NFData PropertyNameSuggestion
