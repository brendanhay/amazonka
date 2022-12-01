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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CodeSigning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.CustomCodeSigning
import Amazonka.IoT.Types.StartSigningJobParameter
import qualified Amazonka.Prelude as Prelude

-- | Describes the method to use when code signing a file.
--
-- /See:/ 'newCodeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { -- | The ID of the @AWSSignerJob@ which was created to sign the file.
    awsSignerJobId :: Prelude.Maybe Prelude.Text,
    -- | Describes the code-signing job.
    startSigningJobParameter :: Prelude.Maybe StartSigningJobParameter,
    -- | A custom method for code signing a file.
    customCodeSigning :: Prelude.Maybe CustomCodeSigning
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
-- 'startSigningJobParameter', 'codeSigning_startSigningJobParameter' - Describes the code-signing job.
--
-- 'customCodeSigning', 'codeSigning_customCodeSigning' - A custom method for code signing a file.
newCodeSigning ::
  CodeSigning
newCodeSigning =
  CodeSigning'
    { awsSignerJobId = Prelude.Nothing,
      startSigningJobParameter = Prelude.Nothing,
      customCodeSigning = Prelude.Nothing
    }

-- | The ID of the @AWSSignerJob@ which was created to sign the file.
codeSigning_awsSignerJobId :: Lens.Lens' CodeSigning (Prelude.Maybe Prelude.Text)
codeSigning_awsSignerJobId = Lens.lens (\CodeSigning' {awsSignerJobId} -> awsSignerJobId) (\s@CodeSigning' {} a -> s {awsSignerJobId = a} :: CodeSigning)

-- | Describes the code-signing job.
codeSigning_startSigningJobParameter :: Lens.Lens' CodeSigning (Prelude.Maybe StartSigningJobParameter)
codeSigning_startSigningJobParameter = Lens.lens (\CodeSigning' {startSigningJobParameter} -> startSigningJobParameter) (\s@CodeSigning' {} a -> s {startSigningJobParameter = a} :: CodeSigning)

-- | A custom method for code signing a file.
codeSigning_customCodeSigning :: Lens.Lens' CodeSigning (Prelude.Maybe CustomCodeSigning)
codeSigning_customCodeSigning = Lens.lens (\CodeSigning' {customCodeSigning} -> customCodeSigning) (\s@CodeSigning' {} a -> s {customCodeSigning = a} :: CodeSigning)

instance Core.FromJSON CodeSigning where
  parseJSON =
    Core.withObject
      "CodeSigning"
      ( \x ->
          CodeSigning'
            Prelude.<$> (x Core..:? "awsSignerJobId")
            Prelude.<*> (x Core..:? "startSigningJobParameter")
            Prelude.<*> (x Core..:? "customCodeSigning")
      )

instance Prelude.Hashable CodeSigning where
  hashWithSalt _salt CodeSigning' {..} =
    _salt `Prelude.hashWithSalt` awsSignerJobId
      `Prelude.hashWithSalt` startSigningJobParameter
      `Prelude.hashWithSalt` customCodeSigning

instance Prelude.NFData CodeSigning where
  rnf CodeSigning' {..} =
    Prelude.rnf awsSignerJobId
      `Prelude.seq` Prelude.rnf startSigningJobParameter
      `Prelude.seq` Prelude.rnf customCodeSigning

instance Core.ToJSON CodeSigning where
  toJSON CodeSigning' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("awsSignerJobId" Core..=)
              Prelude.<$> awsSignerJobId,
            ("startSigningJobParameter" Core..=)
              Prelude.<$> startSigningJobParameter,
            ("customCodeSigning" Core..=)
              Prelude.<$> customCodeSigning
          ]
      )
