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
-- Module      : Amazonka.IoT.Types.CodeSigning
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CodeSigning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CustomCodeSigning
import Amazonka.IoT.Types.StartSigningJobParameter
import qualified Amazonka.Prelude as Prelude

-- | Describes the method to use when code signing a file.
--
-- /See:/ 'newCodeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { -- | The ID of the @AWSSignerJob@ which was created to sign the file.
    awsSignerJobId :: Prelude.Maybe Prelude.Text,
    -- | A custom method for code signing a file.
    customCodeSigning :: Prelude.Maybe CustomCodeSigning,
    -- | Describes the code-signing job.
    startSigningJobParameter :: Prelude.Maybe StartSigningJobParameter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeSigning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsSignerJobId', 'codeSigning_awsSignerJobId' - The ID of the @AWSSignerJob@ which was created to sign the file.
--
-- 'customCodeSigning', 'codeSigning_customCodeSigning' - A custom method for code signing a file.
--
-- 'startSigningJobParameter', 'codeSigning_startSigningJobParameter' - Describes the code-signing job.
newCodeSigning ::
  CodeSigning
newCodeSigning =
  CodeSigning'
    { awsSignerJobId = Prelude.Nothing,
      customCodeSigning = Prelude.Nothing,
      startSigningJobParameter = Prelude.Nothing
    }

-- | The ID of the @AWSSignerJob@ which was created to sign the file.
codeSigning_awsSignerJobId :: Lens.Lens' CodeSigning (Prelude.Maybe Prelude.Text)
codeSigning_awsSignerJobId = Lens.lens (\CodeSigning' {awsSignerJobId} -> awsSignerJobId) (\s@CodeSigning' {} a -> s {awsSignerJobId = a} :: CodeSigning)

-- | A custom method for code signing a file.
codeSigning_customCodeSigning :: Lens.Lens' CodeSigning (Prelude.Maybe CustomCodeSigning)
codeSigning_customCodeSigning = Lens.lens (\CodeSigning' {customCodeSigning} -> customCodeSigning) (\s@CodeSigning' {} a -> s {customCodeSigning = a} :: CodeSigning)

-- | Describes the code-signing job.
codeSigning_startSigningJobParameter :: Lens.Lens' CodeSigning (Prelude.Maybe StartSigningJobParameter)
codeSigning_startSigningJobParameter = Lens.lens (\CodeSigning' {startSigningJobParameter} -> startSigningJobParameter) (\s@CodeSigning' {} a -> s {startSigningJobParameter = a} :: CodeSigning)

instance Data.FromJSON CodeSigning where
  parseJSON =
    Data.withObject
      "CodeSigning"
      ( \x ->
          CodeSigning'
            Prelude.<$> (x Data..:? "awsSignerJobId")
            Prelude.<*> (x Data..:? "customCodeSigning")
            Prelude.<*> (x Data..:? "startSigningJobParameter")
      )

instance Prelude.Hashable CodeSigning where
  hashWithSalt _salt CodeSigning' {..} =
    _salt
      `Prelude.hashWithSalt` awsSignerJobId
      `Prelude.hashWithSalt` customCodeSigning
      `Prelude.hashWithSalt` startSigningJobParameter

instance Prelude.NFData CodeSigning where
  rnf CodeSigning' {..} =
    Prelude.rnf awsSignerJobId
      `Prelude.seq` Prelude.rnf customCodeSigning
      `Prelude.seq` Prelude.rnf startSigningJobParameter

instance Data.ToJSON CodeSigning where
  toJSON CodeSigning' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsSignerJobId" Data..=)
              Prelude.<$> awsSignerJobId,
            ("customCodeSigning" Data..=)
              Prelude.<$> customCodeSigning,
            ("startSigningJobParameter" Data..=)
              Prelude.<$> startSigningJobParameter
          ]
      )
