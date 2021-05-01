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
-- Module      : Network.AWS.SageMaker.Types.SuggestionQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SuggestionQuery where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.PropertyNameQuery

-- | Specified in the GetSearchSuggestions request. Limits the property names
-- that are included in the response.
--
-- /See:/ 'newSuggestionQuery' smart constructor.
data SuggestionQuery = SuggestionQuery'
  { -- | Defines a property name hint. Only property names that begin with the
    -- specified hint are included in the response.
    propertyNameQuery :: Prelude.Maybe PropertyNameQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable SuggestionQuery

instance Prelude.NFData SuggestionQuery

instance Prelude.ToJSON SuggestionQuery where
  toJSON SuggestionQuery' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("PropertyNameQuery" Prelude..=)
              Prelude.<$> propertyNameQuery
          ]
      )
