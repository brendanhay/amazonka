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
-- Module      : Amazonka.SageMaker.Types.SuggestionQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SuggestionQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.PropertyNameQuery

-- | Specified in the GetSearchSuggestions request. Limits the property names
-- that are included in the response.
--
-- /See:/ 'newSuggestionQuery' smart constructor.
data SuggestionQuery = SuggestionQuery'
  { -- | Defines a property name hint. Only property names that begin with the
    -- specified hint are included in the response.
    propertyNameQuery :: Prelude.Maybe PropertyNameQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestionQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyNameQuery', 'suggestionQuery_propertyNameQuery' - Defines a property name hint. Only property names that begin with the
-- specified hint are included in the response.
newSuggestionQuery ::
  SuggestionQuery
newSuggestionQuery =
  SuggestionQuery'
    { propertyNameQuery =
        Prelude.Nothing
    }

-- | Defines a property name hint. Only property names that begin with the
-- specified hint are included in the response.
suggestionQuery_propertyNameQuery :: Lens.Lens' SuggestionQuery (Prelude.Maybe PropertyNameQuery)
suggestionQuery_propertyNameQuery = Lens.lens (\SuggestionQuery' {propertyNameQuery} -> propertyNameQuery) (\s@SuggestionQuery' {} a -> s {propertyNameQuery = a} :: SuggestionQuery)

instance Prelude.Hashable SuggestionQuery where
  hashWithSalt _salt SuggestionQuery' {..} =
    _salt `Prelude.hashWithSalt` propertyNameQuery

instance Prelude.NFData SuggestionQuery where
  rnf SuggestionQuery' {..} =
    Prelude.rnf propertyNameQuery

instance Data.ToJSON SuggestionQuery where
  toJSON SuggestionQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PropertyNameQuery" Data..=)
              Prelude.<$> propertyNameQuery
          ]
      )
