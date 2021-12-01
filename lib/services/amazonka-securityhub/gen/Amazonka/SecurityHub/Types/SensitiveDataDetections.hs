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
-- Module      : Amazonka.SecurityHub.Types.SensitiveDataDetections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SensitiveDataDetections where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.Occurrences

-- | The list of detected instances of sensitive data.
--
-- /See:/ 'newSensitiveDataDetections' smart constructor.
data SensitiveDataDetections = SensitiveDataDetections'
  { -- | Details about the sensitive data that was detected.
    occurrences :: Prelude.Maybe Occurrences,
    -- | The total number of occurrences of sensitive data that were detected.
    count :: Prelude.Maybe Prelude.Integer,
    -- | The type of sensitive data that was detected. For example, the type
    -- might indicate that the data is an email address.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensitiveDataDetections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'occurrences', 'sensitiveDataDetections_occurrences' - Details about the sensitive data that was detected.
--
-- 'count', 'sensitiveDataDetections_count' - The total number of occurrences of sensitive data that were detected.
--
-- 'type'', 'sensitiveDataDetections_type' - The type of sensitive data that was detected. For example, the type
-- might indicate that the data is an email address.
newSensitiveDataDetections ::
  SensitiveDataDetections
newSensitiveDataDetections =
  SensitiveDataDetections'
    { occurrences =
        Prelude.Nothing,
      count = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Details about the sensitive data that was detected.
sensitiveDataDetections_occurrences :: Lens.Lens' SensitiveDataDetections (Prelude.Maybe Occurrences)
sensitiveDataDetections_occurrences = Lens.lens (\SensitiveDataDetections' {occurrences} -> occurrences) (\s@SensitiveDataDetections' {} a -> s {occurrences = a} :: SensitiveDataDetections)

-- | The total number of occurrences of sensitive data that were detected.
sensitiveDataDetections_count :: Lens.Lens' SensitiveDataDetections (Prelude.Maybe Prelude.Integer)
sensitiveDataDetections_count = Lens.lens (\SensitiveDataDetections' {count} -> count) (\s@SensitiveDataDetections' {} a -> s {count = a} :: SensitiveDataDetections)

-- | The type of sensitive data that was detected. For example, the type
-- might indicate that the data is an email address.
sensitiveDataDetections_type :: Lens.Lens' SensitiveDataDetections (Prelude.Maybe Prelude.Text)
sensitiveDataDetections_type = Lens.lens (\SensitiveDataDetections' {type'} -> type') (\s@SensitiveDataDetections' {} a -> s {type' = a} :: SensitiveDataDetections)

instance Core.FromJSON SensitiveDataDetections where
  parseJSON =
    Core.withObject
      "SensitiveDataDetections"
      ( \x ->
          SensitiveDataDetections'
            Prelude.<$> (x Core..:? "Occurrences")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable SensitiveDataDetections where
  hashWithSalt salt' SensitiveDataDetections' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` occurrences

instance Prelude.NFData SensitiveDataDetections where
  rnf SensitiveDataDetections' {..} =
    Prelude.rnf occurrences
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf count

instance Core.ToJSON SensitiveDataDetections where
  toJSON SensitiveDataDetections' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Occurrences" Core..=) Prelude.<$> occurrences,
            ("Count" Core..=) Prelude.<$> count,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )
