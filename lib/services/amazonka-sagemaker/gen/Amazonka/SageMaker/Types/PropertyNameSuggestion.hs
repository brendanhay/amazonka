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
-- Module      : Amazonka.SageMaker.Types.PropertyNameSuggestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PropertyNameSuggestion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A property name returned from a @GetSearchSuggestions@ call that
-- specifies a value in the @PropertyNameQuery@ field.
--
-- /See:/ 'newPropertyNameSuggestion' smart constructor.
data PropertyNameSuggestion = PropertyNameSuggestion'
  { -- | A suggested property name based on what you entered in the search
    -- textbox in the Amazon SageMaker console.
    propertyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON PropertyNameSuggestion where
  parseJSON =
    Data.withObject
      "PropertyNameSuggestion"
      ( \x ->
          PropertyNameSuggestion'
            Prelude.<$> (x Data..:? "PropertyName")
      )

instance Prelude.Hashable PropertyNameSuggestion where
  hashWithSalt _salt PropertyNameSuggestion' {..} =
    _salt `Prelude.hashWithSalt` propertyName

instance Prelude.NFData PropertyNameSuggestion where
  rnf PropertyNameSuggestion' {..} =
    Prelude.rnf propertyName
