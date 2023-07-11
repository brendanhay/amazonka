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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.DefaultDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.Occurrences
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a type of sensitive data that was detected by
-- a managed data identifier and produced a sensitive data finding.
--
-- /See:/ 'newDefaultDetection' smart constructor.
data DefaultDetection = DefaultDetection'
  { -- | The total number of occurrences of the type of sensitive data that was
    -- detected.
    count :: Prelude.Maybe Prelude.Integer,
    -- | The location of 1-15 occurrences of the sensitive data that was
    -- detected. A finding includes location data for a maximum of 15
    -- occurrences of sensitive data.
    occurrences :: Prelude.Maybe Occurrences,
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
-- 'count', 'defaultDetection_count' - The total number of occurrences of the type of sensitive data that was
-- detected.
--
-- 'occurrences', 'defaultDetection_occurrences' - The location of 1-15 occurrences of the sensitive data that was
-- detected. A finding includes location data for a maximum of 15
-- occurrences of sensitive data.
--
-- 'type'', 'defaultDetection_type' - The type of sensitive data that was detected. For example,
-- AWS_CREDENTIALS, PHONE_NUMBER, or ADDRESS.
newDefaultDetection ::
  DefaultDetection
newDefaultDetection =
  DefaultDetection'
    { count = Prelude.Nothing,
      occurrences = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The total number of occurrences of the type of sensitive data that was
-- detected.
defaultDetection_count :: Lens.Lens' DefaultDetection (Prelude.Maybe Prelude.Integer)
defaultDetection_count = Lens.lens (\DefaultDetection' {count} -> count) (\s@DefaultDetection' {} a -> s {count = a} :: DefaultDetection)

-- | The location of 1-15 occurrences of the sensitive data that was
-- detected. A finding includes location data for a maximum of 15
-- occurrences of sensitive data.
defaultDetection_occurrences :: Lens.Lens' DefaultDetection (Prelude.Maybe Occurrences)
defaultDetection_occurrences = Lens.lens (\DefaultDetection' {occurrences} -> occurrences) (\s@DefaultDetection' {} a -> s {occurrences = a} :: DefaultDetection)

-- | The type of sensitive data that was detected. For example,
-- AWS_CREDENTIALS, PHONE_NUMBER, or ADDRESS.
defaultDetection_type :: Lens.Lens' DefaultDetection (Prelude.Maybe Prelude.Text)
defaultDetection_type = Lens.lens (\DefaultDetection' {type'} -> type') (\s@DefaultDetection' {} a -> s {type' = a} :: DefaultDetection)

instance Data.FromJSON DefaultDetection where
  parseJSON =
    Data.withObject
      "DefaultDetection"
      ( \x ->
          DefaultDetection'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "occurrences")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable DefaultDetection where
  hashWithSalt _salt DefaultDetection' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` occurrences
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DefaultDetection where
  rnf DefaultDetection' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf occurrences
      `Prelude.seq` Prelude.rnf type'
