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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryS3Output
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryS3Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains output information for protected queries with an S3 output
-- type.
--
-- /See:/ 'newProtectedQueryS3Output' smart constructor.
data ProtectedQueryS3Output = ProtectedQueryS3Output'
  { -- | The S3 location of the result.
    location :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryS3Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'protectedQueryS3Output_location' - The S3 location of the result.
newProtectedQueryS3Output ::
  -- | 'location'
  Prelude.Text ->
  ProtectedQueryS3Output
newProtectedQueryS3Output pLocation_ =
  ProtectedQueryS3Output' {location = pLocation_}

-- | The S3 location of the result.
protectedQueryS3Output_location :: Lens.Lens' ProtectedQueryS3Output Prelude.Text
protectedQueryS3Output_location = Lens.lens (\ProtectedQueryS3Output' {location} -> location) (\s@ProtectedQueryS3Output' {} a -> s {location = a} :: ProtectedQueryS3Output)

instance Data.FromJSON ProtectedQueryS3Output where
  parseJSON =
    Data.withObject
      "ProtectedQueryS3Output"
      ( \x ->
          ProtectedQueryS3Output'
            Prelude.<$> (x Data..: "location")
      )

instance Prelude.Hashable ProtectedQueryS3Output where
  hashWithSalt _salt ProtectedQueryS3Output' {..} =
    _salt `Prelude.hashWithSalt` location

instance Prelude.NFData ProtectedQueryS3Output where
  rnf ProtectedQueryS3Output' {..} =
    Prelude.rnf location
