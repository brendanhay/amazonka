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
-- Module      : Amazonka.MacieV2.Types.AllowListSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AllowListSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a subset of information about an allow list.
--
-- /See:/ 'newAllowListSummary' smart constructor.
data AllowListSummary = AllowListSummary'
  { -- | The Amazon Resource Name (ARN) of the allow list.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the allow
    -- list was created in Amazon Macie.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The custom description of the allow list.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the allow list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the allow list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the allow
    -- list\'s settings were most recently changed in Amazon Macie.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowListSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'allowListSummary_arn' - The Amazon Resource Name (ARN) of the allow list.
--
-- 'createdAt', 'allowListSummary_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the allow
-- list was created in Amazon Macie.
--
-- 'description', 'allowListSummary_description' - The custom description of the allow list.
--
-- 'id', 'allowListSummary_id' - The unique identifier for the allow list.
--
-- 'name', 'allowListSummary_name' - The custom name of the allow list.
--
-- 'updatedAt', 'allowListSummary_updatedAt' - The date and time, in UTC and extended ISO 8601 format, when the allow
-- list\'s settings were most recently changed in Amazon Macie.
newAllowListSummary ::
  AllowListSummary
newAllowListSummary =
  AllowListSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the allow list.
allowListSummary_arn :: Lens.Lens' AllowListSummary (Prelude.Maybe Prelude.Text)
allowListSummary_arn = Lens.lens (\AllowListSummary' {arn} -> arn) (\s@AllowListSummary' {} a -> s {arn = a} :: AllowListSummary)

-- | The date and time, in UTC and extended ISO 8601 format, when the allow
-- list was created in Amazon Macie.
allowListSummary_createdAt :: Lens.Lens' AllowListSummary (Prelude.Maybe Prelude.UTCTime)
allowListSummary_createdAt = Lens.lens (\AllowListSummary' {createdAt} -> createdAt) (\s@AllowListSummary' {} a -> s {createdAt = a} :: AllowListSummary) Prelude.. Lens.mapping Data._Time

-- | The custom description of the allow list.
allowListSummary_description :: Lens.Lens' AllowListSummary (Prelude.Maybe Prelude.Text)
allowListSummary_description = Lens.lens (\AllowListSummary' {description} -> description) (\s@AllowListSummary' {} a -> s {description = a} :: AllowListSummary)

-- | The unique identifier for the allow list.
allowListSummary_id :: Lens.Lens' AllowListSummary (Prelude.Maybe Prelude.Text)
allowListSummary_id = Lens.lens (\AllowListSummary' {id} -> id) (\s@AllowListSummary' {} a -> s {id = a} :: AllowListSummary)

-- | The custom name of the allow list.
allowListSummary_name :: Lens.Lens' AllowListSummary (Prelude.Maybe Prelude.Text)
allowListSummary_name = Lens.lens (\AllowListSummary' {name} -> name) (\s@AllowListSummary' {} a -> s {name = a} :: AllowListSummary)

-- | The date and time, in UTC and extended ISO 8601 format, when the allow
-- list\'s settings were most recently changed in Amazon Macie.
allowListSummary_updatedAt :: Lens.Lens' AllowListSummary (Prelude.Maybe Prelude.UTCTime)
allowListSummary_updatedAt = Lens.lens (\AllowListSummary' {updatedAt} -> updatedAt) (\s@AllowListSummary' {} a -> s {updatedAt = a} :: AllowListSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AllowListSummary where
  parseJSON =
    Data.withObject
      "AllowListSummary"
      ( \x ->
          AllowListSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable AllowListSummary where
  hashWithSalt _salt AllowListSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData AllowListSummary where
  rnf AllowListSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedAt
