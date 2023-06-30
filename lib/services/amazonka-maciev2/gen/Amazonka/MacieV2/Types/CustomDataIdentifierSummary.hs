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
-- Module      : Amazonka.MacieV2.Types.CustomDataIdentifierSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.CustomDataIdentifierSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a custom data identifier.
--
-- /See:/ 'newCustomDataIdentifierSummary' smart constructor.
data CustomDataIdentifierSummary = CustomDataIdentifierSummary'
  { -- | The Amazon Resource Name (ARN) of the custom data identifier.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the custom
    -- data identifier was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The custom description of the custom data identifier.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the custom data identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the custom data identifier.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDataIdentifierSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'customDataIdentifierSummary_arn' - The Amazon Resource Name (ARN) of the custom data identifier.
--
-- 'createdAt', 'customDataIdentifierSummary_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
--
-- 'description', 'customDataIdentifierSummary_description' - The custom description of the custom data identifier.
--
-- 'id', 'customDataIdentifierSummary_id' - The unique identifier for the custom data identifier.
--
-- 'name', 'customDataIdentifierSummary_name' - The custom name of the custom data identifier.
newCustomDataIdentifierSummary ::
  CustomDataIdentifierSummary
newCustomDataIdentifierSummary =
  CustomDataIdentifierSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom data identifier.
customDataIdentifierSummary_arn :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_arn = Lens.lens (\CustomDataIdentifierSummary' {arn} -> arn) (\s@CustomDataIdentifierSummary' {} a -> s {arn = a} :: CustomDataIdentifierSummary)

-- | The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
customDataIdentifierSummary_createdAt :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.UTCTime)
customDataIdentifierSummary_createdAt = Lens.lens (\CustomDataIdentifierSummary' {createdAt} -> createdAt) (\s@CustomDataIdentifierSummary' {} a -> s {createdAt = a} :: CustomDataIdentifierSummary) Prelude.. Lens.mapping Data._Time

-- | The custom description of the custom data identifier.
customDataIdentifierSummary_description :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_description = Lens.lens (\CustomDataIdentifierSummary' {description} -> description) (\s@CustomDataIdentifierSummary' {} a -> s {description = a} :: CustomDataIdentifierSummary)

-- | The unique identifier for the custom data identifier.
customDataIdentifierSummary_id :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_id = Lens.lens (\CustomDataIdentifierSummary' {id} -> id) (\s@CustomDataIdentifierSummary' {} a -> s {id = a} :: CustomDataIdentifierSummary)

-- | The custom name of the custom data identifier.
customDataIdentifierSummary_name :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_name = Lens.lens (\CustomDataIdentifierSummary' {name} -> name) (\s@CustomDataIdentifierSummary' {} a -> s {name = a} :: CustomDataIdentifierSummary)

instance Data.FromJSON CustomDataIdentifierSummary where
  parseJSON =
    Data.withObject
      "CustomDataIdentifierSummary"
      ( \x ->
          CustomDataIdentifierSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable CustomDataIdentifierSummary where
  hashWithSalt _salt CustomDataIdentifierSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData CustomDataIdentifierSummary where
  rnf CustomDataIdentifierSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
