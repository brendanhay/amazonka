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
-- Module      : Amazonka.ChimeSDKMessaging.Types.SearchField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.SearchField where

import Amazonka.ChimeSDKMessaging.Types.SearchFieldKey
import Amazonka.ChimeSDKMessaging.Types.SearchFieldOperator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A @Field@ of the channel that you want to search.
--
-- /See:/ 'newSearchField' smart constructor.
data SearchField = SearchField'
  { -- | An @enum@ value that indicates the key to search the channel on.
    -- @MEMBERS@ allows you to search channels based on memberships. You can
    -- use it with the @EQUALS@ operator to get channels whose memberships are
    -- equal to the specified values, and with the @INCLUDES@ operator to get
    -- channels whose memberships include the specified values.
    key :: SearchFieldKey,
    -- | The values that you want to search for, a list of strings. The values
    -- must be @AppInstanceUserArns@ specified as a list of strings.
    --
    -- This operation isn\'t supported for @AppInstanceUsers@ with large number
    -- of memberships.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator used to compare field values, currently @EQUALS@ or
    -- @INCLUDES@. Use the @EQUALS@ operator to find channels whose memberships
    -- equal the specified values. Use the @INCLUDES@ operator to find channels
    -- whose memberships include the specified values.
    operator :: SearchFieldOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'searchField_key' - An @enum@ value that indicates the key to search the channel on.
-- @MEMBERS@ allows you to search channels based on memberships. You can
-- use it with the @EQUALS@ operator to get channels whose memberships are
-- equal to the specified values, and with the @INCLUDES@ operator to get
-- channels whose memberships include the specified values.
--
-- 'values', 'searchField_values' - The values that you want to search for, a list of strings. The values
-- must be @AppInstanceUserArns@ specified as a list of strings.
--
-- This operation isn\'t supported for @AppInstanceUsers@ with large number
-- of memberships.
--
-- 'operator', 'searchField_operator' - The operator used to compare field values, currently @EQUALS@ or
-- @INCLUDES@. Use the @EQUALS@ operator to find channels whose memberships
-- equal the specified values. Use the @INCLUDES@ operator to find channels
-- whose memberships include the specified values.
newSearchField ::
  -- | 'key'
  SearchFieldKey ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  SearchFieldOperator ->
  SearchField
newSearchField pKey_ pValues_ pOperator_ =
  SearchField'
    { key = pKey_,
      values = Lens.coerced Lens.# pValues_,
      operator = pOperator_
    }

-- | An @enum@ value that indicates the key to search the channel on.
-- @MEMBERS@ allows you to search channels based on memberships. You can
-- use it with the @EQUALS@ operator to get channels whose memberships are
-- equal to the specified values, and with the @INCLUDES@ operator to get
-- channels whose memberships include the specified values.
searchField_key :: Lens.Lens' SearchField SearchFieldKey
searchField_key = Lens.lens (\SearchField' {key} -> key) (\s@SearchField' {} a -> s {key = a} :: SearchField)

-- | The values that you want to search for, a list of strings. The values
-- must be @AppInstanceUserArns@ specified as a list of strings.
--
-- This operation isn\'t supported for @AppInstanceUsers@ with large number
-- of memberships.
searchField_values :: Lens.Lens' SearchField (Prelude.NonEmpty Prelude.Text)
searchField_values = Lens.lens (\SearchField' {values} -> values) (\s@SearchField' {} a -> s {values = a} :: SearchField) Prelude.. Lens.coerced

-- | The operator used to compare field values, currently @EQUALS@ or
-- @INCLUDES@. Use the @EQUALS@ operator to find channels whose memberships
-- equal the specified values. Use the @INCLUDES@ operator to find channels
-- whose memberships include the specified values.
searchField_operator :: Lens.Lens' SearchField SearchFieldOperator
searchField_operator = Lens.lens (\SearchField' {operator} -> operator) (\s@SearchField' {} a -> s {operator = a} :: SearchField)

instance Prelude.Hashable SearchField where
  hashWithSalt _salt SearchField' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData SearchField where
  rnf SearchField' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON SearchField where
  toJSON SearchField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values),
            Prelude.Just ("Operator" Data..= operator)
          ]
      )
