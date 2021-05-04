{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.CodeSigning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigning where

import Network.AWS.IoT.Types.CustomCodeSigning
import Network.AWS.IoT.Types.StartSigningJobParameter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the method to use when code signing a file.
--
-- /See:/ 'newCodeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { -- | Describes the code-signing job.
    startSigningJobParameter :: Prelude.Maybe StartSigningJobParameter,
    -- | The ID of the AWSSignerJob which was created to sign the file.
    awsSignerJobId :: Prelude.Maybe Prelude.Text,
    -- | A custom method for code signing a file.
    customCodeSigning :: Prelude.Maybe CustomCodeSigning
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CodeSigning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startSigningJobParameter', 'codeSigning_startSigningJobParameter' - Describes the code-signing job.
--
-- 'awsSignerJobId', 'codeSigning_awsSignerJobId' - The ID of the AWSSignerJob which was created to sign the file.
--
-- 'customCodeSigning', 'codeSigning_customCodeSigning' - A custom method for code signing a file.
newCodeSigning ::
  CodeSigning
newCodeSigning =
  CodeSigning'
    { startSigningJobParameter =
        Prelude.Nothing,
      awsSignerJobId = Prelude.Nothing,
      customCodeSigning = Prelude.Nothing
    }

-- | Describes the code-signing job.
codeSigning_startSigningJobParameter :: Lens.Lens' CodeSigning (Prelude.Maybe StartSigningJobParameter)
codeSigning_startSigningJobParameter = Lens.lens (\CodeSigning' {startSigningJobParameter} -> startSigningJobParameter) (\s@CodeSigning' {} a -> s {startSigningJobParameter = a} :: CodeSigning)

-- | The ID of the AWSSignerJob which was created to sign the file.
codeSigning_awsSignerJobId :: Lens.Lens' CodeSigning (Prelude.Maybe Prelude.Text)
codeSigning_awsSignerJobId = Lens.lens (\CodeSigning' {awsSignerJobId} -> awsSignerJobId) (\s@CodeSigning' {} a -> s {awsSignerJobId = a} :: CodeSigning)

-- | A custom method for code signing a file.
codeSigning_customCodeSigning :: Lens.Lens' CodeSigning (Prelude.Maybe CustomCodeSigning)
codeSigning_customCodeSigning = Lens.lens (\CodeSigning' {customCodeSigning} -> customCodeSigning) (\s@CodeSigning' {} a -> s {customCodeSigning = a} :: CodeSigning)

instance Prelude.FromJSON CodeSigning where
  parseJSON =
    Prelude.withObject
      "CodeSigning"
      ( \x ->
          CodeSigning'
            Prelude.<$> (x Prelude..:? "startSigningJobParameter")
            Prelude.<*> (x Prelude..:? "awsSignerJobId")
            Prelude.<*> (x Prelude..:? "customCodeSigning")
      )

instance Prelude.Hashable CodeSigning

instance Prelude.NFData CodeSigning

instance Prelude.ToJSON CodeSigning where
  toJSON CodeSigning' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("startSigningJobParameter" Prelude..=)
              Prelude.<$> startSigningJobParameter,
            ("awsSignerJobId" Prelude..=)
              Prelude.<$> awsSignerJobId,
            ("customCodeSigning" Prelude..=)
              Prelude.<$> customCodeSigning
          ]
      )
