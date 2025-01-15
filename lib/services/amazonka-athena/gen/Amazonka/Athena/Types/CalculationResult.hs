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
-- Module      : Amazonka.Athena.Types.CalculationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CalculationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an application-specific calculation result.
--
-- /See:/ 'newCalculationResult' smart constructor.
data CalculationResult = CalculationResult'
  { -- | The Amazon S3 location of the folder for the calculation results.
    resultS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The data format of the calculation result.
    resultType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of the @stderr@ error messages file for the
    -- calculation.
    stdErrorS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of the @stdout@ file for the calculation.
    stdOutS3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultS3Uri', 'calculationResult_resultS3Uri' - The Amazon S3 location of the folder for the calculation results.
--
-- 'resultType', 'calculationResult_resultType' - The data format of the calculation result.
--
-- 'stdErrorS3Uri', 'calculationResult_stdErrorS3Uri' - The Amazon S3 location of the @stderr@ error messages file for the
-- calculation.
--
-- 'stdOutS3Uri', 'calculationResult_stdOutS3Uri' - The Amazon S3 location of the @stdout@ file for the calculation.
newCalculationResult ::
  CalculationResult
newCalculationResult =
  CalculationResult'
    { resultS3Uri = Prelude.Nothing,
      resultType = Prelude.Nothing,
      stdErrorS3Uri = Prelude.Nothing,
      stdOutS3Uri = Prelude.Nothing
    }

-- | The Amazon S3 location of the folder for the calculation results.
calculationResult_resultS3Uri :: Lens.Lens' CalculationResult (Prelude.Maybe Prelude.Text)
calculationResult_resultS3Uri = Lens.lens (\CalculationResult' {resultS3Uri} -> resultS3Uri) (\s@CalculationResult' {} a -> s {resultS3Uri = a} :: CalculationResult)

-- | The data format of the calculation result.
calculationResult_resultType :: Lens.Lens' CalculationResult (Prelude.Maybe Prelude.Text)
calculationResult_resultType = Lens.lens (\CalculationResult' {resultType} -> resultType) (\s@CalculationResult' {} a -> s {resultType = a} :: CalculationResult)

-- | The Amazon S3 location of the @stderr@ error messages file for the
-- calculation.
calculationResult_stdErrorS3Uri :: Lens.Lens' CalculationResult (Prelude.Maybe Prelude.Text)
calculationResult_stdErrorS3Uri = Lens.lens (\CalculationResult' {stdErrorS3Uri} -> stdErrorS3Uri) (\s@CalculationResult' {} a -> s {stdErrorS3Uri = a} :: CalculationResult)

-- | The Amazon S3 location of the @stdout@ file for the calculation.
calculationResult_stdOutS3Uri :: Lens.Lens' CalculationResult (Prelude.Maybe Prelude.Text)
calculationResult_stdOutS3Uri = Lens.lens (\CalculationResult' {stdOutS3Uri} -> stdOutS3Uri) (\s@CalculationResult' {} a -> s {stdOutS3Uri = a} :: CalculationResult)

instance Data.FromJSON CalculationResult where
  parseJSON =
    Data.withObject
      "CalculationResult"
      ( \x ->
          CalculationResult'
            Prelude.<$> (x Data..:? "ResultS3Uri")
            Prelude.<*> (x Data..:? "ResultType")
            Prelude.<*> (x Data..:? "StdErrorS3Uri")
            Prelude.<*> (x Data..:? "StdOutS3Uri")
      )

instance Prelude.Hashable CalculationResult where
  hashWithSalt _salt CalculationResult' {..} =
    _salt
      `Prelude.hashWithSalt` resultS3Uri
      `Prelude.hashWithSalt` resultType
      `Prelude.hashWithSalt` stdErrorS3Uri
      `Prelude.hashWithSalt` stdOutS3Uri

instance Prelude.NFData CalculationResult where
  rnf CalculationResult' {..} =
    Prelude.rnf resultS3Uri `Prelude.seq`
      Prelude.rnf resultType `Prelude.seq`
        Prelude.rnf stdErrorS3Uri `Prelude.seq`
          Prelude.rnf stdOutS3Uri
