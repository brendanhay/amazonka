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
-- Module      : Amazonka.MacieV2.Types.BatchGetCustomDataIdentifierSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BatchGetCustomDataIdentifierSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a custom data identifier.
--
-- /See:/ 'newBatchGetCustomDataIdentifierSummary' smart constructor.
data BatchGetCustomDataIdentifierSummary = BatchGetCustomDataIdentifierSummary'
  { -- | The Amazon Resource Name (ARN) of the custom data identifier.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the custom
    -- data identifier was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | Specifies whether the custom data identifier was deleted. If you delete
    -- a custom data identifier, Amazon Macie doesn\'t delete it permanently.
    -- Instead, it soft deletes the identifier.
    deleted :: Prelude.Maybe Prelude.Bool,
    -- | The custom description of the custom data identifier.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the custom data identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the custom data identifier.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetCustomDataIdentifierSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'batchGetCustomDataIdentifierSummary_arn' - The Amazon Resource Name (ARN) of the custom data identifier.
--
-- 'createdAt', 'batchGetCustomDataIdentifierSummary_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
--
-- 'deleted', 'batchGetCustomDataIdentifierSummary_deleted' - Specifies whether the custom data identifier was deleted. If you delete
-- a custom data identifier, Amazon Macie doesn\'t delete it permanently.
-- Instead, it soft deletes the identifier.
--
-- 'description', 'batchGetCustomDataIdentifierSummary_description' - The custom description of the custom data identifier.
--
-- 'id', 'batchGetCustomDataIdentifierSummary_id' - The unique identifier for the custom data identifier.
--
-- 'name', 'batchGetCustomDataIdentifierSummary_name' - The custom name of the custom data identifier.
newBatchGetCustomDataIdentifierSummary ::
  BatchGetCustomDataIdentifierSummary
newBatchGetCustomDataIdentifierSummary =
  BatchGetCustomDataIdentifierSummary'
    { arn =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      deleted = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom data identifier.
batchGetCustomDataIdentifierSummary_arn :: Lens.Lens' BatchGetCustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
batchGetCustomDataIdentifierSummary_arn = Lens.lens (\BatchGetCustomDataIdentifierSummary' {arn} -> arn) (\s@BatchGetCustomDataIdentifierSummary' {} a -> s {arn = a} :: BatchGetCustomDataIdentifierSummary)

-- | The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
batchGetCustomDataIdentifierSummary_createdAt :: Lens.Lens' BatchGetCustomDataIdentifierSummary (Prelude.Maybe Prelude.UTCTime)
batchGetCustomDataIdentifierSummary_createdAt = Lens.lens (\BatchGetCustomDataIdentifierSummary' {createdAt} -> createdAt) (\s@BatchGetCustomDataIdentifierSummary' {} a -> s {createdAt = a} :: BatchGetCustomDataIdentifierSummary) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the custom data identifier was deleted. If you delete
-- a custom data identifier, Amazon Macie doesn\'t delete it permanently.
-- Instead, it soft deletes the identifier.
batchGetCustomDataIdentifierSummary_deleted :: Lens.Lens' BatchGetCustomDataIdentifierSummary (Prelude.Maybe Prelude.Bool)
batchGetCustomDataIdentifierSummary_deleted = Lens.lens (\BatchGetCustomDataIdentifierSummary' {deleted} -> deleted) (\s@BatchGetCustomDataIdentifierSummary' {} a -> s {deleted = a} :: BatchGetCustomDataIdentifierSummary)

-- | The custom description of the custom data identifier.
batchGetCustomDataIdentifierSummary_description :: Lens.Lens' BatchGetCustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
batchGetCustomDataIdentifierSummary_description = Lens.lens (\BatchGetCustomDataIdentifierSummary' {description} -> description) (\s@BatchGetCustomDataIdentifierSummary' {} a -> s {description = a} :: BatchGetCustomDataIdentifierSummary)

-- | The unique identifier for the custom data identifier.
batchGetCustomDataIdentifierSummary_id :: Lens.Lens' BatchGetCustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
batchGetCustomDataIdentifierSummary_id = Lens.lens (\BatchGetCustomDataIdentifierSummary' {id} -> id) (\s@BatchGetCustomDataIdentifierSummary' {} a -> s {id = a} :: BatchGetCustomDataIdentifierSummary)

-- | The custom name of the custom data identifier.
batchGetCustomDataIdentifierSummary_name :: Lens.Lens' BatchGetCustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
batchGetCustomDataIdentifierSummary_name = Lens.lens (\BatchGetCustomDataIdentifierSummary' {name} -> name) (\s@BatchGetCustomDataIdentifierSummary' {} a -> s {name = a} :: BatchGetCustomDataIdentifierSummary)

instance
  Data.FromJSON
    BatchGetCustomDataIdentifierSummary
  where
  parseJSON =
    Data.withObject
      "BatchGetCustomDataIdentifierSummary"
      ( \x ->
          BatchGetCustomDataIdentifierSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "deleted")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    BatchGetCustomDataIdentifierSummary
  where
  hashWithSalt
    _salt
    BatchGetCustomDataIdentifierSummary' {..} =
      _salt `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` deleted
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    BatchGetCustomDataIdentifierSummary
  where
  rnf BatchGetCustomDataIdentifierSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf deleted
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
