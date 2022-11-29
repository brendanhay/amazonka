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
-- Module      : Amazonka.SecurityHub.Types.CustomDataIdentifiersDetections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.CustomDataIdentifiersDetections where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.Occurrences

-- | The list of detected instances of sensitive data.
--
-- /See:/ 'newCustomDataIdentifiersDetections' smart constructor.
data CustomDataIdentifiersDetections = CustomDataIdentifiersDetections'
  { -- | Details about the sensitive data that was detected.
    occurrences :: Prelude.Maybe Occurrences,
    -- | he name of the custom identifier that detected the sensitive data.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the custom identifier that was used to detect the sensitive
    -- data.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The total number of occurrences of sensitive data that were detected.
    count :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDataIdentifiersDetections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'occurrences', 'customDataIdentifiersDetections_occurrences' - Details about the sensitive data that was detected.
--
-- 'name', 'customDataIdentifiersDetections_name' - he name of the custom identifier that detected the sensitive data.
--
-- 'arn', 'customDataIdentifiersDetections_arn' - The ARN of the custom identifier that was used to detect the sensitive
-- data.
--
-- 'count', 'customDataIdentifiersDetections_count' - The total number of occurrences of sensitive data that were detected.
newCustomDataIdentifiersDetections ::
  CustomDataIdentifiersDetections
newCustomDataIdentifiersDetections =
  CustomDataIdentifiersDetections'
    { occurrences =
        Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | Details about the sensitive data that was detected.
customDataIdentifiersDetections_occurrences :: Lens.Lens' CustomDataIdentifiersDetections (Prelude.Maybe Occurrences)
customDataIdentifiersDetections_occurrences = Lens.lens (\CustomDataIdentifiersDetections' {occurrences} -> occurrences) (\s@CustomDataIdentifiersDetections' {} a -> s {occurrences = a} :: CustomDataIdentifiersDetections)

-- | he name of the custom identifier that detected the sensitive data.
customDataIdentifiersDetections_name :: Lens.Lens' CustomDataIdentifiersDetections (Prelude.Maybe Prelude.Text)
customDataIdentifiersDetections_name = Lens.lens (\CustomDataIdentifiersDetections' {name} -> name) (\s@CustomDataIdentifiersDetections' {} a -> s {name = a} :: CustomDataIdentifiersDetections)

-- | The ARN of the custom identifier that was used to detect the sensitive
-- data.
customDataIdentifiersDetections_arn :: Lens.Lens' CustomDataIdentifiersDetections (Prelude.Maybe Prelude.Text)
customDataIdentifiersDetections_arn = Lens.lens (\CustomDataIdentifiersDetections' {arn} -> arn) (\s@CustomDataIdentifiersDetections' {} a -> s {arn = a} :: CustomDataIdentifiersDetections)

-- | The total number of occurrences of sensitive data that were detected.
customDataIdentifiersDetections_count :: Lens.Lens' CustomDataIdentifiersDetections (Prelude.Maybe Prelude.Integer)
customDataIdentifiersDetections_count = Lens.lens (\CustomDataIdentifiersDetections' {count} -> count) (\s@CustomDataIdentifiersDetections' {} a -> s {count = a} :: CustomDataIdentifiersDetections)

instance
  Core.FromJSON
    CustomDataIdentifiersDetections
  where
  parseJSON =
    Core.withObject
      "CustomDataIdentifiersDetections"
      ( \x ->
          CustomDataIdentifiersDetections'
            Prelude.<$> (x Core..:? "Occurrences")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Count")
      )

instance
  Prelude.Hashable
    CustomDataIdentifiersDetections
  where
  hashWithSalt
    _salt
    CustomDataIdentifiersDetections' {..} =
      _salt `Prelude.hashWithSalt` occurrences
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` count

instance
  Prelude.NFData
    CustomDataIdentifiersDetections
  where
  rnf CustomDataIdentifiersDetections' {..} =
    Prelude.rnf occurrences
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf count

instance Core.ToJSON CustomDataIdentifiersDetections where
  toJSON CustomDataIdentifiersDetections' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Occurrences" Core..=) Prelude.<$> occurrences,
            ("Name" Core..=) Prelude.<$> name,
            ("Arn" Core..=) Prelude.<$> arn,
            ("Count" Core..=) Prelude.<$> count
          ]
      )
