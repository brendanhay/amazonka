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
-- Module      : Amazonka.MacieV2.Types.DefaultDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.DefaultDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.Occurrences
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a type of sensitive data that was detected by
-- a managed data identifier and produced a sensitive data finding.
--
-- /See:/ 'newDefaultDetection' smart constructor.
data DefaultDetection = DefaultDetection'
  { -- | The location of 1-15 occurrences of the sensitive data that was
    -- detected. A finding includes location data for a maximum of 15
    -- occurrences of sensitive data.
    occurrences :: Prelude.Maybe Occurrences,
    -- | The total number of occurrences of the type of sensitive data that was
    -- detected.
    count :: Prelude.Maybe Prelude.Integer,
    -- | The type of sensitive data that was detected. For example,
    -- AWS_CREDENTIALS, PHONE_NUMBER, or ADDRESS.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'occurrences', 'defaultDetection_occurrences' - The location of 1-15 occurrences of the sensitive data that was
-- detected. A finding includes location data for a maximum of 15
-- occurrences of sensitive data.
--
-- 'count', 'defaultDetection_count' - The total number of occurrences of the type of sensitive data that was
-- detected.
--
-- 'type'', 'defaultDetection_type' - The type of sensitive data that was detected. For example,
-- AWS_CREDENTIALS, PHONE_NUMBER, or ADDRESS.
newDefaultDetection ::
  DefaultDetection
newDefaultDetection =
  DefaultDetection'
    { occurrences = Prelude.Nothing,
      count = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The location of 1-15 occurrences of the sensitive data that was
-- detected. A finding includes location data for a maximum of 15
-- occurrences of sensitive data.
defaultDetection_occurrences :: Lens.Lens' DefaultDetection (Prelude.Maybe Occurrences)
defaultDetection_occurrences = Lens.lens (\DefaultDetection' {occurrences} -> occurrences) (\s@DefaultDetection' {} a -> s {occurrences = a} :: DefaultDetection)

-- | The total number of occurrences of the type of sensitive data that was
-- detected.
defaultDetection_count :: Lens.Lens' DefaultDetection (Prelude.Maybe Prelude.Integer)
defaultDetection_count = Lens.lens (\DefaultDetection' {count} -> count) (\s@DefaultDetection' {} a -> s {count = a} :: DefaultDetection)

-- | The type of sensitive data that was detected. For example,
-- AWS_CREDENTIALS, PHONE_NUMBER, or ADDRESS.
defaultDetection_type :: Lens.Lens' DefaultDetection (Prelude.Maybe Prelude.Text)
defaultDetection_type = Lens.lens (\DefaultDetection' {type'} -> type') (\s@DefaultDetection' {} a -> s {type' = a} :: DefaultDetection)

instance Core.FromJSON DefaultDetection where
  parseJSON =
    Core.withObject
      "DefaultDetection"
      ( \x ->
          DefaultDetection'
            Prelude.<$> (x Core..:? "occurrences")
            Prelude.<*> (x Core..:? "count")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable DefaultDetection where
  hashWithSalt _salt DefaultDetection' {..} =
    _salt `Prelude.hashWithSalt` occurrences
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DefaultDetection where
  rnf DefaultDetection' {..} =
    Prelude.rnf occurrences
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf type'
