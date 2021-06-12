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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.CustomCodeSigning
import Network.AWS.IoT.Types.StartSigningJobParameter
import qualified Network.AWS.Lens as Lens

-- | Describes the method to use when code signing a file.
--
-- /See:/ 'newCodeSigning' smart constructor.
data CodeSigning = CodeSigning'
  { -- | Describes the code-signing job.
    startSigningJobParameter :: Core.Maybe StartSigningJobParameter,
    -- | The ID of the AWSSignerJob which was created to sign the file.
    awsSignerJobId :: Core.Maybe Core.Text,
    -- | A custom method for code signing a file.
    customCodeSigning :: Core.Maybe CustomCodeSigning
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      awsSignerJobId = Core.Nothing,
      customCodeSigning = Core.Nothing
    }

-- | Describes the code-signing job.
codeSigning_startSigningJobParameter :: Lens.Lens' CodeSigning (Core.Maybe StartSigningJobParameter)
codeSigning_startSigningJobParameter = Lens.lens (\CodeSigning' {startSigningJobParameter} -> startSigningJobParameter) (\s@CodeSigning' {} a -> s {startSigningJobParameter = a} :: CodeSigning)

-- | The ID of the AWSSignerJob which was created to sign the file.
codeSigning_awsSignerJobId :: Lens.Lens' CodeSigning (Core.Maybe Core.Text)
codeSigning_awsSignerJobId = Lens.lens (\CodeSigning' {awsSignerJobId} -> awsSignerJobId) (\s@CodeSigning' {} a -> s {awsSignerJobId = a} :: CodeSigning)

-- | A custom method for code signing a file.
codeSigning_customCodeSigning :: Lens.Lens' CodeSigning (Core.Maybe CustomCodeSigning)
codeSigning_customCodeSigning = Lens.lens (\CodeSigning' {customCodeSigning} -> customCodeSigning) (\s@CodeSigning' {} a -> s {customCodeSigning = a} :: CodeSigning)

instance Core.FromJSON CodeSigning where
  parseJSON =
    Core.withObject
      "CodeSigning"
      ( \x ->
          CodeSigning'
            Core.<$> (x Core..:? "startSigningJobParameter")
            Core.<*> (x Core..:? "awsSignerJobId")
            Core.<*> (x Core..:? "customCodeSigning")
      )

instance Core.Hashable CodeSigning

instance Core.NFData CodeSigning

instance Core.ToJSON CodeSigning where
  toJSON CodeSigning' {..} =
    Core.object
      ( Core.catMaybes
          [ ("startSigningJobParameter" Core..=)
              Core.<$> startSigningJobParameter,
            ("awsSignerJobId" Core..=) Core.<$> awsSignerJobId,
            ("customCodeSigning" Core..=)
              Core.<$> customCodeSigning
          ]
      )
