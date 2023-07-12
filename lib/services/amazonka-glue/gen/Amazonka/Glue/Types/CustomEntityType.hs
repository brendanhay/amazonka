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
-- Module      : Amazonka.Glue.Types.CustomEntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CustomEntityType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing a custom pattern for detecting sensitive data
-- across the columns and rows of your structured data.
--
-- /See:/ 'newCustomEntityType' smart constructor.
data CustomEntityType = CustomEntityType'
  { -- | A list of context words. If none of these context words are found within
    -- the vicinity of the regular expression the data will not be detected as
    -- sensitive data.
    --
    -- If no context words are passed only a regular expression is checked.
    contextWords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A name for the custom pattern that allows it to be retrieved or deleted
    -- later. This name must be unique per Amazon Web Services account.
    name :: Prelude.Text,
    -- | A regular expression string that is used for detecting sensitive data in
    -- a custom pattern.
    regexString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomEntityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextWords', 'customEntityType_contextWords' - A list of context words. If none of these context words are found within
-- the vicinity of the regular expression the data will not be detected as
-- sensitive data.
--
-- If no context words are passed only a regular expression is checked.
--
-- 'name', 'customEntityType_name' - A name for the custom pattern that allows it to be retrieved or deleted
-- later. This name must be unique per Amazon Web Services account.
--
-- 'regexString', 'customEntityType_regexString' - A regular expression string that is used for detecting sensitive data in
-- a custom pattern.
newCustomEntityType ::
  -- | 'name'
  Prelude.Text ->
  -- | 'regexString'
  Prelude.Text ->
  CustomEntityType
newCustomEntityType pName_ pRegexString_ =
  CustomEntityType'
    { contextWords = Prelude.Nothing,
      name = pName_,
      regexString = pRegexString_
    }

-- | A list of context words. If none of these context words are found within
-- the vicinity of the regular expression the data will not be detected as
-- sensitive data.
--
-- If no context words are passed only a regular expression is checked.
customEntityType_contextWords :: Lens.Lens' CustomEntityType (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
customEntityType_contextWords = Lens.lens (\CustomEntityType' {contextWords} -> contextWords) (\s@CustomEntityType' {} a -> s {contextWords = a} :: CustomEntityType) Prelude.. Lens.mapping Lens.coerced

-- | A name for the custom pattern that allows it to be retrieved or deleted
-- later. This name must be unique per Amazon Web Services account.
customEntityType_name :: Lens.Lens' CustomEntityType Prelude.Text
customEntityType_name = Lens.lens (\CustomEntityType' {name} -> name) (\s@CustomEntityType' {} a -> s {name = a} :: CustomEntityType)

-- | A regular expression string that is used for detecting sensitive data in
-- a custom pattern.
customEntityType_regexString :: Lens.Lens' CustomEntityType Prelude.Text
customEntityType_regexString = Lens.lens (\CustomEntityType' {regexString} -> regexString) (\s@CustomEntityType' {} a -> s {regexString = a} :: CustomEntityType)

instance Data.FromJSON CustomEntityType where
  parseJSON =
    Data.withObject
      "CustomEntityType"
      ( \x ->
          CustomEntityType'
            Prelude.<$> (x Data..:? "ContextWords")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "RegexString")
      )

instance Prelude.Hashable CustomEntityType where
  hashWithSalt _salt CustomEntityType' {..} =
    _salt
      `Prelude.hashWithSalt` contextWords
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regexString

instance Prelude.NFData CustomEntityType where
  rnf CustomEntityType' {..} =
    Prelude.rnf contextWords
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regexString
