{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentArtifact
  ( TrialComponentArtifact (..),

    -- * Smart constructor
    mkTrialComponentArtifact,

    -- * Lenses
    tcaMediaType,
    tcaValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an input or output artifact of a trial component. You specify @TrialComponentArtifact@ as part of the @InputArtifacts@ and @OutputArtifacts@ parameters in the 'CreateTrialComponent' request.
--
-- Examples of input artifacts are datasets, algorithms, hyperparameters, source code, and instance types. Examples of output artifacts are metrics, snapshots, logs, and images.
--
-- /See:/ 'mkTrialComponentArtifact' smart constructor.
data TrialComponentArtifact = TrialComponentArtifact'
  { -- | The media type of the artifact, which indicates the type of data in the artifact file. The media type consists of a /type/ and a /subtype/ concatenated with a slash (/) character, for example, text/csv, image/jpeg, and s3/uri. The type specifies the category of the media. The subtype specifies the kind of data.
    mediaType :: Lude.Maybe Lude.Text,
    -- | The location of the artifact.
    value :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponentArtifact' with the minimum fields required to make a request.
--
-- * 'mediaType' - The media type of the artifact, which indicates the type of data in the artifact file. The media type consists of a /type/ and a /subtype/ concatenated with a slash (/) character, for example, text/csv, image/jpeg, and s3/uri. The type specifies the category of the media. The subtype specifies the kind of data.
-- * 'value' - The location of the artifact.
mkTrialComponentArtifact ::
  -- | 'value'
  Lude.Text ->
  TrialComponentArtifact
mkTrialComponentArtifact pValue_ =
  TrialComponentArtifact'
    { mediaType = Lude.Nothing,
      value = pValue_
    }

-- | The media type of the artifact, which indicates the type of data in the artifact file. The media type consists of a /type/ and a /subtype/ concatenated with a slash (/) character, for example, text/csv, image/jpeg, and s3/uri. The type specifies the category of the media. The subtype specifies the kind of data.
--
-- /Note:/ Consider using 'mediaType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaMediaType :: Lens.Lens' TrialComponentArtifact (Lude.Maybe Lude.Text)
tcaMediaType = Lens.lens (mediaType :: TrialComponentArtifact -> Lude.Maybe Lude.Text) (\s a -> s {mediaType = a} :: TrialComponentArtifact)
{-# DEPRECATED tcaMediaType "Use generic-lens or generic-optics with 'mediaType' instead." #-}

-- | The location of the artifact.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcaValue :: Lens.Lens' TrialComponentArtifact Lude.Text
tcaValue = Lens.lens (value :: TrialComponentArtifact -> Lude.Text) (\s a -> s {value = a} :: TrialComponentArtifact)
{-# DEPRECATED tcaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON TrialComponentArtifact where
  parseJSON =
    Lude.withObject
      "TrialComponentArtifact"
      ( \x ->
          TrialComponentArtifact'
            Lude.<$> (x Lude..:? "MediaType") Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON TrialComponentArtifact where
  toJSON TrialComponentArtifact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MediaType" Lude..=) Lude.<$> mediaType,
            Lude.Just ("Value" Lude..= value)
          ]
      )
