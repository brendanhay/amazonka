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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.CustomDataIdentifierSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a custom data identifier.
--
-- /See:/ 'newCustomDataIdentifierSummary' smart constructor.
data CustomDataIdentifierSummary = CustomDataIdentifierSummary'
  { -- | The custom name of the custom data identifier.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom data identifier.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The custom description of the custom data identifier.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the custom data identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the custom
    -- data identifier was created.
    createdAt :: Prelude.Maybe Core.POSIX
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
-- 'name', 'customDataIdentifierSummary_name' - The custom name of the custom data identifier.
--
-- 'arn', 'customDataIdentifierSummary_arn' - The Amazon Resource Name (ARN) of the custom data identifier.
--
-- 'description', 'customDataIdentifierSummary_description' - The custom description of the custom data identifier.
--
-- 'id', 'customDataIdentifierSummary_id' - The unique identifier for the custom data identifier.
--
-- 'createdAt', 'customDataIdentifierSummary_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
newCustomDataIdentifierSummary ::
  CustomDataIdentifierSummary
newCustomDataIdentifierSummary =
  CustomDataIdentifierSummary'
    { name =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The custom name of the custom data identifier.
customDataIdentifierSummary_name :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_name = Lens.lens (\CustomDataIdentifierSummary' {name} -> name) (\s@CustomDataIdentifierSummary' {} a -> s {name = a} :: CustomDataIdentifierSummary)

-- | The Amazon Resource Name (ARN) of the custom data identifier.
customDataIdentifierSummary_arn :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_arn = Lens.lens (\CustomDataIdentifierSummary' {arn} -> arn) (\s@CustomDataIdentifierSummary' {} a -> s {arn = a} :: CustomDataIdentifierSummary)

-- | The custom description of the custom data identifier.
customDataIdentifierSummary_description :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_description = Lens.lens (\CustomDataIdentifierSummary' {description} -> description) (\s@CustomDataIdentifierSummary' {} a -> s {description = a} :: CustomDataIdentifierSummary)

-- | The unique identifier for the custom data identifier.
customDataIdentifierSummary_id :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.Text)
customDataIdentifierSummary_id = Lens.lens (\CustomDataIdentifierSummary' {id} -> id) (\s@CustomDataIdentifierSummary' {} a -> s {id = a} :: CustomDataIdentifierSummary)

-- | The date and time, in UTC and extended ISO 8601 format, when the custom
-- data identifier was created.
customDataIdentifierSummary_createdAt :: Lens.Lens' CustomDataIdentifierSummary (Prelude.Maybe Prelude.UTCTime)
customDataIdentifierSummary_createdAt = Lens.lens (\CustomDataIdentifierSummary' {createdAt} -> createdAt) (\s@CustomDataIdentifierSummary' {} a -> s {createdAt = a} :: CustomDataIdentifierSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CustomDataIdentifierSummary where
  parseJSON =
    Core.withObject
      "CustomDataIdentifierSummary"
      ( \x ->
          CustomDataIdentifierSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable CustomDataIdentifierSummary where
  hashWithSalt _salt CustomDataIdentifierSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData CustomDataIdentifierSummary where
  rnf CustomDataIdentifierSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
