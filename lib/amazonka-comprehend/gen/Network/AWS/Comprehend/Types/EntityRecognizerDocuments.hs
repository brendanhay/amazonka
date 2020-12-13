{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerDocuments
  ( EntityRecognizerDocuments (..),

    -- * Smart constructor
    mkEntityRecognizerDocuments,

    -- * Lenses
    erdS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the training documents submitted with an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerDocuments' smart constructor.
newtype EntityRecognizerDocuments = EntityRecognizerDocuments'
  { -- | Specifies the Amazon S3 location where the training documents for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
    s3URI :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityRecognizerDocuments' with the minimum fields required to make a request.
--
-- * 's3URI' - Specifies the Amazon S3 location where the training documents for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
mkEntityRecognizerDocuments ::
  -- | 's3URI'
  Lude.Text ->
  EntityRecognizerDocuments
mkEntityRecognizerDocuments pS3URI_ =
  EntityRecognizerDocuments' {s3URI = pS3URI_}

-- | Specifies the Amazon S3 location where the training documents for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdS3URI :: Lens.Lens' EntityRecognizerDocuments Lude.Text
erdS3URI = Lens.lens (s3URI :: EntityRecognizerDocuments -> Lude.Text) (\s a -> s {s3URI = a} :: EntityRecognizerDocuments)
{-# DEPRECATED erdS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON EntityRecognizerDocuments where
  parseJSON =
    Lude.withObject
      "EntityRecognizerDocuments"
      (\x -> EntityRecognizerDocuments' Lude.<$> (x Lude..: "S3Uri"))

instance Lude.ToJSON EntityRecognizerDocuments where
  toJSON EntityRecognizerDocuments' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("S3Uri" Lude..= s3URI)])
