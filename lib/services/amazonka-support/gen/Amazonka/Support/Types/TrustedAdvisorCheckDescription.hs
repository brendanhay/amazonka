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
-- Module      : Amazonka.Support.Types.TrustedAdvisorCheckDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.TrustedAdvisorCheckDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The description and metadata for a Trusted Advisor check.
--
-- /See:/ 'newTrustedAdvisorCheckDescription' smart constructor.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription'
  { -- | The unique identifier for the Trusted Advisor check.
    id :: Prelude.Text,
    -- | The display name for the Trusted Advisor check.
    name :: Prelude.Text,
    -- | The description of the Trusted Advisor check, which includes the alert
    -- criteria and recommended operations (contains HTML markup).
    description :: Prelude.Text,
    -- | The category of the Trusted Advisor check.
    category :: Prelude.Text,
    -- | The column headings for the data returned by the Trusted Advisor check.
    -- The order of the headings corresponds to the order of the data in the
    -- __Metadata__ element of the TrustedAdvisorResourceDetail for the check.
    -- __Metadata__ contains all the data that is shown in the Excel download,
    -- even in those cases where the UI shows just summary data.
    metadata :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorCheckDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'trustedAdvisorCheckDescription_id' - The unique identifier for the Trusted Advisor check.
--
-- 'name', 'trustedAdvisorCheckDescription_name' - The display name for the Trusted Advisor check.
--
-- 'description', 'trustedAdvisorCheckDescription_description' - The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended operations (contains HTML markup).
--
-- 'category', 'trustedAdvisorCheckDescription_category' - The category of the Trusted Advisor check.
--
-- 'metadata', 'trustedAdvisorCheckDescription_metadata' - The column headings for the data returned by the Trusted Advisor check.
-- The order of the headings corresponds to the order of the data in the
-- __Metadata__ element of the TrustedAdvisorResourceDetail for the check.
-- __Metadata__ contains all the data that is shown in the Excel download,
-- even in those cases where the UI shows just summary data.
newTrustedAdvisorCheckDescription ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'category'
  Prelude.Text ->
  TrustedAdvisorCheckDescription
newTrustedAdvisorCheckDescription
  pId_
  pName_
  pDescription_
  pCategory_ =
    TrustedAdvisorCheckDescription'
      { id = pId_,
        name = pName_,
        description = pDescription_,
        category = pCategory_,
        metadata = Prelude.mempty
      }

-- | The unique identifier for the Trusted Advisor check.
trustedAdvisorCheckDescription_id :: Lens.Lens' TrustedAdvisorCheckDescription Prelude.Text
trustedAdvisorCheckDescription_id = Lens.lens (\TrustedAdvisorCheckDescription' {id} -> id) (\s@TrustedAdvisorCheckDescription' {} a -> s {id = a} :: TrustedAdvisorCheckDescription)

-- | The display name for the Trusted Advisor check.
trustedAdvisorCheckDescription_name :: Lens.Lens' TrustedAdvisorCheckDescription Prelude.Text
trustedAdvisorCheckDescription_name = Lens.lens (\TrustedAdvisorCheckDescription' {name} -> name) (\s@TrustedAdvisorCheckDescription' {} a -> s {name = a} :: TrustedAdvisorCheckDescription)

-- | The description of the Trusted Advisor check, which includes the alert
-- criteria and recommended operations (contains HTML markup).
trustedAdvisorCheckDescription_description :: Lens.Lens' TrustedAdvisorCheckDescription Prelude.Text
trustedAdvisorCheckDescription_description = Lens.lens (\TrustedAdvisorCheckDescription' {description} -> description) (\s@TrustedAdvisorCheckDescription' {} a -> s {description = a} :: TrustedAdvisorCheckDescription)

-- | The category of the Trusted Advisor check.
trustedAdvisorCheckDescription_category :: Lens.Lens' TrustedAdvisorCheckDescription Prelude.Text
trustedAdvisorCheckDescription_category = Lens.lens (\TrustedAdvisorCheckDescription' {category} -> category) (\s@TrustedAdvisorCheckDescription' {} a -> s {category = a} :: TrustedAdvisorCheckDescription)

-- | The column headings for the data returned by the Trusted Advisor check.
-- The order of the headings corresponds to the order of the data in the
-- __Metadata__ element of the TrustedAdvisorResourceDetail for the check.
-- __Metadata__ contains all the data that is shown in the Excel download,
-- even in those cases where the UI shows just summary data.
trustedAdvisorCheckDescription_metadata :: Lens.Lens' TrustedAdvisorCheckDescription [Prelude.Text]
trustedAdvisorCheckDescription_metadata = Lens.lens (\TrustedAdvisorCheckDescription' {metadata} -> metadata) (\s@TrustedAdvisorCheckDescription' {} a -> s {metadata = a} :: TrustedAdvisorCheckDescription) Prelude.. Lens.coerced

instance Data.FromJSON TrustedAdvisorCheckDescription where
  parseJSON =
    Data.withObject
      "TrustedAdvisorCheckDescription"
      ( \x ->
          TrustedAdvisorCheckDescription'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "category")
            Prelude.<*> (x Data..:? "metadata" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    TrustedAdvisorCheckDescription
  where
  hashWithSalt
    _salt
    TrustedAdvisorCheckDescription' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` category
        `Prelude.hashWithSalt` metadata

instance
  Prelude.NFData
    TrustedAdvisorCheckDescription
  where
  rnf TrustedAdvisorCheckDescription' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf category `Prelude.seq`
            Prelude.rnf metadata
