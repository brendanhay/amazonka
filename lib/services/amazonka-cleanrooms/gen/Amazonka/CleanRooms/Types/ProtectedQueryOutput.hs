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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryOutput where

import Amazonka.CleanRooms.Types.ProtectedQueryS3Output
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the protected query output.
--
-- /See:/ 'newProtectedQueryOutput' smart constructor.
data ProtectedQueryOutput = ProtectedQueryOutput'
  { -- | If present, the output for a protected query with an \`S3\` output type.
    s3 :: Prelude.Maybe ProtectedQueryS3Output
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'protectedQueryOutput_s3' - If present, the output for a protected query with an \`S3\` output type.
newProtectedQueryOutput ::
  ProtectedQueryOutput
newProtectedQueryOutput =
  ProtectedQueryOutput' {s3 = Prelude.Nothing}

-- | If present, the output for a protected query with an \`S3\` output type.
protectedQueryOutput_s3 :: Lens.Lens' ProtectedQueryOutput (Prelude.Maybe ProtectedQueryS3Output)
protectedQueryOutput_s3 = Lens.lens (\ProtectedQueryOutput' {s3} -> s3) (\s@ProtectedQueryOutput' {} a -> s {s3 = a} :: ProtectedQueryOutput)

instance Data.FromJSON ProtectedQueryOutput where
  parseJSON =
    Data.withObject
      "ProtectedQueryOutput"
      ( \x ->
          ProtectedQueryOutput' Prelude.<$> (x Data..:? "s3")
      )

instance Prelude.Hashable ProtectedQueryOutput where
  hashWithSalt _salt ProtectedQueryOutput' {..} =
    _salt `Prelude.hashWithSalt` s3

instance Prelude.NFData ProtectedQueryOutput where
  rnf ProtectedQueryOutput' {..} = Prelude.rnf s3
