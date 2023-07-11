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
-- Module      : Amazonka.SageMaker.Types.TrialComponentArtifact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentArtifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an input or output artifact of a trial component. You specify
-- @TrialComponentArtifact@ as part of the @InputArtifacts@ and
-- @OutputArtifacts@ parameters in the CreateTrialComponent request.
--
-- Examples of input artifacts are datasets, algorithms, hyperparameters,
-- source code, and instance types. Examples of output artifacts are
-- metrics, snapshots, logs, and images.
--
-- /See:/ 'newTrialComponentArtifact' smart constructor.
data TrialComponentArtifact = TrialComponentArtifact'
  { -- | The media type of the artifact, which indicates the type of data in the
    -- artifact file. The media type consists of a /type/ and a /subtype/
    -- concatenated with a slash (\/) character, for example, text\/csv,
    -- image\/jpeg, and s3\/uri. The type specifies the category of the media.
    -- The subtype specifies the kind of data.
    mediaType :: Prelude.Maybe Prelude.Text,
    -- | The location of the artifact.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaType', 'trialComponentArtifact_mediaType' - The media type of the artifact, which indicates the type of data in the
-- artifact file. The media type consists of a /type/ and a /subtype/
-- concatenated with a slash (\/) character, for example, text\/csv,
-- image\/jpeg, and s3\/uri. The type specifies the category of the media.
-- The subtype specifies the kind of data.
--
-- 'value', 'trialComponentArtifact_value' - The location of the artifact.
newTrialComponentArtifact ::
  -- | 'value'
  Prelude.Text ->
  TrialComponentArtifact
newTrialComponentArtifact pValue_ =
  TrialComponentArtifact'
    { mediaType =
        Prelude.Nothing,
      value = pValue_
    }

-- | The media type of the artifact, which indicates the type of data in the
-- artifact file. The media type consists of a /type/ and a /subtype/
-- concatenated with a slash (\/) character, for example, text\/csv,
-- image\/jpeg, and s3\/uri. The type specifies the category of the media.
-- The subtype specifies the kind of data.
trialComponentArtifact_mediaType :: Lens.Lens' TrialComponentArtifact (Prelude.Maybe Prelude.Text)
trialComponentArtifact_mediaType = Lens.lens (\TrialComponentArtifact' {mediaType} -> mediaType) (\s@TrialComponentArtifact' {} a -> s {mediaType = a} :: TrialComponentArtifact)

-- | The location of the artifact.
trialComponentArtifact_value :: Lens.Lens' TrialComponentArtifact Prelude.Text
trialComponentArtifact_value = Lens.lens (\TrialComponentArtifact' {value} -> value) (\s@TrialComponentArtifact' {} a -> s {value = a} :: TrialComponentArtifact)

instance Data.FromJSON TrialComponentArtifact where
  parseJSON =
    Data.withObject
      "TrialComponentArtifact"
      ( \x ->
          TrialComponentArtifact'
            Prelude.<$> (x Data..:? "MediaType")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable TrialComponentArtifact where
  hashWithSalt _salt TrialComponentArtifact' {..} =
    _salt
      `Prelude.hashWithSalt` mediaType
      `Prelude.hashWithSalt` value

instance Prelude.NFData TrialComponentArtifact where
  rnf TrialComponentArtifact' {..} =
    Prelude.rnf mediaType
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON TrialComponentArtifact where
  toJSON TrialComponentArtifact' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MediaType" Data..=) Prelude.<$> mediaType,
            Prelude.Just ("Value" Data..= value)
          ]
      )
