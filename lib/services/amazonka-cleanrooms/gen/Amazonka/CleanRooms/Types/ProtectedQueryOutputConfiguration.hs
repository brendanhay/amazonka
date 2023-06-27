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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryOutputConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryOutputConfiguration where

import Amazonka.CleanRooms.Types.ProtectedQueryS3OutputConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration details for protected query output.
--
-- /See:/ 'newProtectedQueryOutputConfiguration' smart constructor.
data ProtectedQueryOutputConfiguration = ProtectedQueryOutputConfiguration'
  { -- | Required configuration for a protected query with an \`S3\` output type.
    s3 :: Prelude.Maybe ProtectedQueryS3OutputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryOutputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'protectedQueryOutputConfiguration_s3' - Required configuration for a protected query with an \`S3\` output type.
newProtectedQueryOutputConfiguration ::
  ProtectedQueryOutputConfiguration
newProtectedQueryOutputConfiguration =
  ProtectedQueryOutputConfiguration'
    { s3 =
        Prelude.Nothing
    }

-- | Required configuration for a protected query with an \`S3\` output type.
protectedQueryOutputConfiguration_s3 :: Lens.Lens' ProtectedQueryOutputConfiguration (Prelude.Maybe ProtectedQueryS3OutputConfiguration)
protectedQueryOutputConfiguration_s3 = Lens.lens (\ProtectedQueryOutputConfiguration' {s3} -> s3) (\s@ProtectedQueryOutputConfiguration' {} a -> s {s3 = a} :: ProtectedQueryOutputConfiguration)

instance
  Data.FromJSON
    ProtectedQueryOutputConfiguration
  where
  parseJSON =
    Data.withObject
      "ProtectedQueryOutputConfiguration"
      ( \x ->
          ProtectedQueryOutputConfiguration'
            Prelude.<$> (x Data..:? "s3")
      )

instance
  Prelude.Hashable
    ProtectedQueryOutputConfiguration
  where
  hashWithSalt
    _salt
    ProtectedQueryOutputConfiguration' {..} =
      _salt `Prelude.hashWithSalt` s3

instance
  Prelude.NFData
    ProtectedQueryOutputConfiguration
  where
  rnf ProtectedQueryOutputConfiguration' {..} =
    Prelude.rnf s3

instance
  Data.ToJSON
    ProtectedQueryOutputConfiguration
  where
  toJSON ProtectedQueryOutputConfiguration' {..} =
    Data.object
      (Prelude.catMaybes [("s3" Data..=) Prelude.<$> s3])
