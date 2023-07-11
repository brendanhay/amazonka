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
-- Module      : Amazonka.Connect.Types.HoursOfOperationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HoursOfOperationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about hours of operation for a contact
-- center.
--
-- /See:/ 'newHoursOfOperationSummary' smart constructor.
data HoursOfOperationSummary = HoursOfOperationSummary'
  { -- | The Amazon Resource Name (ARN) of the hours of operation.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the hours of operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the hours of operation.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoursOfOperationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'hoursOfOperationSummary_arn' - The Amazon Resource Name (ARN) of the hours of operation.
--
-- 'id', 'hoursOfOperationSummary_id' - The identifier of the hours of operation.
--
-- 'name', 'hoursOfOperationSummary_name' - The name of the hours of operation.
newHoursOfOperationSummary ::
  HoursOfOperationSummary
newHoursOfOperationSummary =
  HoursOfOperationSummary'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hours of operation.
hoursOfOperationSummary_arn :: Lens.Lens' HoursOfOperationSummary (Prelude.Maybe Prelude.Text)
hoursOfOperationSummary_arn = Lens.lens (\HoursOfOperationSummary' {arn} -> arn) (\s@HoursOfOperationSummary' {} a -> s {arn = a} :: HoursOfOperationSummary)

-- | The identifier of the hours of operation.
hoursOfOperationSummary_id :: Lens.Lens' HoursOfOperationSummary (Prelude.Maybe Prelude.Text)
hoursOfOperationSummary_id = Lens.lens (\HoursOfOperationSummary' {id} -> id) (\s@HoursOfOperationSummary' {} a -> s {id = a} :: HoursOfOperationSummary)

-- | The name of the hours of operation.
hoursOfOperationSummary_name :: Lens.Lens' HoursOfOperationSummary (Prelude.Maybe Prelude.Text)
hoursOfOperationSummary_name = Lens.lens (\HoursOfOperationSummary' {name} -> name) (\s@HoursOfOperationSummary' {} a -> s {name = a} :: HoursOfOperationSummary)

instance Data.FromJSON HoursOfOperationSummary where
  parseJSON =
    Data.withObject
      "HoursOfOperationSummary"
      ( \x ->
          HoursOfOperationSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable HoursOfOperationSummary where
  hashWithSalt _salt HoursOfOperationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData HoursOfOperationSummary where
  rnf HoursOfOperationSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
