{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
  ( EntityRecognizerAnnotations (..),

    -- * Smart constructor
    mkEntityRecognizerAnnotations,

    -- * Lenses
    eraS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the annotations associated with a entity recognizer.
--
-- /See:/ 'mkEntityRecognizerAnnotations' smart constructor.
newtype EntityRecognizerAnnotations = EntityRecognizerAnnotations'
  { -- | Specifies the Amazon S3 location where the annotations for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
    s3URI :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityRecognizerAnnotations' with the minimum fields required to make a request.
--
-- * 's3URI' - Specifies the Amazon S3 location where the annotations for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
mkEntityRecognizerAnnotations ::
  -- | 's3URI'
  Lude.Text ->
  EntityRecognizerAnnotations
mkEntityRecognizerAnnotations pS3URI_ =
  EntityRecognizerAnnotations' {s3URI = pS3URI_}

-- | Specifies the Amazon S3 location where the annotations for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eraS3URI :: Lens.Lens' EntityRecognizerAnnotations Lude.Text
eraS3URI = Lens.lens (s3URI :: EntityRecognizerAnnotations -> Lude.Text) (\s a -> s {s3URI = a} :: EntityRecognizerAnnotations)
{-# DEPRECATED eraS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON EntityRecognizerAnnotations where
  parseJSON =
    Lude.withObject
      "EntityRecognizerAnnotations"
      (\x -> EntityRecognizerAnnotations' Lude.<$> (x Lude..: "S3Uri"))

instance Lude.ToJSON EntityRecognizerAnnotations where
  toJSON EntityRecognizerAnnotations' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("S3Uri" Lude..= s3URI)])
