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
-- Module      : Amazonka.SMS.Types.SSMOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.SSMOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.S3Location

-- | Contains the location of validation output.
--
-- /See:/ 'newSSMOutput' smart constructor.
data SSMOutput = SSMOutput'
  { s3Location :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SSMOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'sSMOutput_s3Location' - Undocumented member.
newSSMOutput ::
  SSMOutput
newSSMOutput =
  SSMOutput' {s3Location = Prelude.Nothing}

-- | Undocumented member.
sSMOutput_s3Location :: Lens.Lens' SSMOutput (Prelude.Maybe S3Location)
sSMOutput_s3Location = Lens.lens (\SSMOutput' {s3Location} -> s3Location) (\s@SSMOutput' {} a -> s {s3Location = a} :: SSMOutput)

instance Data.FromJSON SSMOutput where
  parseJSON =
    Data.withObject
      "SSMOutput"
      ( \x ->
          SSMOutput' Prelude.<$> (x Data..:? "s3Location")
      )

instance Prelude.Hashable SSMOutput where
  hashWithSalt _salt SSMOutput' {..} =
    _salt `Prelude.hashWithSalt` s3Location

instance Prelude.NFData SSMOutput where
  rnf SSMOutput' {..} = Prelude.rnf s3Location
