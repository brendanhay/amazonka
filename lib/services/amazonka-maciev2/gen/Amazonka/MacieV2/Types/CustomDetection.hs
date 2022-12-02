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
-- Module      : Amazonka.MacieV2.Types.CustomDetection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.CustomDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.Occurrences
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a custom data identifier that produced a
-- sensitive data finding, and the sensitive data that it detected for the
-- finding.
--
-- /See:/ 'newCustomDetection' smart constructor.
data CustomDetection = CustomDetection'
  { -- | The location of 1-15 occurrences of the sensitive data that the custom
    -- data identifier detected. A finding includes location data for a maximum
    -- of 15 occurrences of sensitive data.
    occurrences :: Prelude.Maybe Occurrences,
    -- | The name of the custom data identifier.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the custom data identifier.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The total number of occurrences of the sensitive data that the custom
    -- data identifier detected.
    count :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'occurrences', 'customDetection_occurrences' - The location of 1-15 occurrences of the sensitive data that the custom
-- data identifier detected. A finding includes location data for a maximum
-- of 15 occurrences of sensitive data.
--
-- 'name', 'customDetection_name' - The name of the custom data identifier.
--
-- 'arn', 'customDetection_arn' - The Amazon Resource Name (ARN) of the custom data identifier.
--
-- 'count', 'customDetection_count' - The total number of occurrences of the sensitive data that the custom
-- data identifier detected.
newCustomDetection ::
  CustomDetection
newCustomDetection =
  CustomDetection'
    { occurrences = Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The location of 1-15 occurrences of the sensitive data that the custom
-- data identifier detected. A finding includes location data for a maximum
-- of 15 occurrences of sensitive data.
customDetection_occurrences :: Lens.Lens' CustomDetection (Prelude.Maybe Occurrences)
customDetection_occurrences = Lens.lens (\CustomDetection' {occurrences} -> occurrences) (\s@CustomDetection' {} a -> s {occurrences = a} :: CustomDetection)

-- | The name of the custom data identifier.
customDetection_name :: Lens.Lens' CustomDetection (Prelude.Maybe Prelude.Text)
customDetection_name = Lens.lens (\CustomDetection' {name} -> name) (\s@CustomDetection' {} a -> s {name = a} :: CustomDetection)

-- | The Amazon Resource Name (ARN) of the custom data identifier.
customDetection_arn :: Lens.Lens' CustomDetection (Prelude.Maybe Prelude.Text)
customDetection_arn = Lens.lens (\CustomDetection' {arn} -> arn) (\s@CustomDetection' {} a -> s {arn = a} :: CustomDetection)

-- | The total number of occurrences of the sensitive data that the custom
-- data identifier detected.
customDetection_count :: Lens.Lens' CustomDetection (Prelude.Maybe Prelude.Integer)
customDetection_count = Lens.lens (\CustomDetection' {count} -> count) (\s@CustomDetection' {} a -> s {count = a} :: CustomDetection)

instance Data.FromJSON CustomDetection where
  parseJSON =
    Data.withObject
      "CustomDetection"
      ( \x ->
          CustomDetection'
            Prelude.<$> (x Data..:? "occurrences")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "count")
      )

instance Prelude.Hashable CustomDetection where
  hashWithSalt _salt CustomDetection' {..} =
    _salt `Prelude.hashWithSalt` occurrences
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` count

instance Prelude.NFData CustomDetection where
  rnf CustomDetection' {..} =
    Prelude.rnf occurrences
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf count
