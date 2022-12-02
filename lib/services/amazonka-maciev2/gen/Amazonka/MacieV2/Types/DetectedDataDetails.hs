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
-- Module      : Amazonka.MacieV2.Types.DetectedDataDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.DetectedDataDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies 1-10 occurrences of a specific type of sensitive data reported
-- by a finding.
--
-- /See:/ 'newDetectedDataDetails' smart constructor.
data DetectedDataDetails = DetectedDataDetails'
  { -- | An occurrence of the specified type of sensitive data. Each occurrence
    -- can contain 1-128 characters.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedDataDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'detectedDataDetails_value' - An occurrence of the specified type of sensitive data. Each occurrence
-- can contain 1-128 characters.
newDetectedDataDetails ::
  -- | 'value'
  Prelude.Text ->
  DetectedDataDetails
newDetectedDataDetails pValue_ =
  DetectedDataDetails' {value = pValue_}

-- | An occurrence of the specified type of sensitive data. Each occurrence
-- can contain 1-128 characters.
detectedDataDetails_value :: Lens.Lens' DetectedDataDetails Prelude.Text
detectedDataDetails_value = Lens.lens (\DetectedDataDetails' {value} -> value) (\s@DetectedDataDetails' {} a -> s {value = a} :: DetectedDataDetails)

instance Data.FromJSON DetectedDataDetails where
  parseJSON =
    Data.withObject
      "DetectedDataDetails"
      ( \x ->
          DetectedDataDetails' Prelude.<$> (x Data..: "value")
      )

instance Prelude.Hashable DetectedDataDetails where
  hashWithSalt _salt DetectedDataDetails' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData DetectedDataDetails where
  rnf DetectedDataDetails' {..} = Prelude.rnf value
