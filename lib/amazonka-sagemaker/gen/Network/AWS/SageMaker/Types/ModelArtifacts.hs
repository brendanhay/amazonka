-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelArtifacts
  ( ModelArtifacts (..),

    -- * Smart constructor
    mkModelArtifacts,

    -- * Lenses
    maS3ModelArtifacts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the location that is configured for storing model artifacts.
--
-- Model artifacts are the output that results from training a model, and typically consist of trained parameters, a model defintion that desribes how to compute inferences, and other metadata.
--
-- /See:/ 'mkModelArtifacts' smart constructor.
newtype ModelArtifacts = ModelArtifacts'
  { s3ModelArtifacts ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelArtifacts' with the minimum fields required to make a request.
--
-- * 's3ModelArtifacts' - The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
mkModelArtifacts ::
  -- | 's3ModelArtifacts'
  Lude.Text ->
  ModelArtifacts
mkModelArtifacts pS3ModelArtifacts_ =
  ModelArtifacts' {s3ModelArtifacts = pS3ModelArtifacts_}

-- | The path of the S3 object that contains the model artifacts. For example, @s3://bucket-name/keynameprefix/model.tar.gz@ .
--
-- /Note:/ Consider using 's3ModelArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maS3ModelArtifacts :: Lens.Lens' ModelArtifacts Lude.Text
maS3ModelArtifacts = Lens.lens (s3ModelArtifacts :: ModelArtifacts -> Lude.Text) (\s a -> s {s3ModelArtifacts = a} :: ModelArtifacts)
{-# DEPRECATED maS3ModelArtifacts "Use generic-lens or generic-optics with 's3ModelArtifacts' instead." #-}

instance Lude.FromJSON ModelArtifacts where
  parseJSON =
    Lude.withObject
      "ModelArtifacts"
      (\x -> ModelArtifacts' Lude.<$> (x Lude..: "S3ModelArtifacts"))
