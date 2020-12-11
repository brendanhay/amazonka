-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerEntityList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerEntityList
  ( EntityRecognizerEntityList (..),

    -- * Smart constructor
    mkEntityRecognizerEntityList,

    -- * Lenses
    erelS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the entity recognizer submitted with an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerEntityList' smart constructor.
newtype EntityRecognizerEntityList = EntityRecognizerEntityList'
  { s3URI ::
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

-- | Creates a value of 'EntityRecognizerEntityList' with the minimum fields required to make a request.
--
-- * 's3URI' - Specifies the Amazon S3 location where the entity list is located. The URI must be in the same region as the API endpoint that you are calling.
mkEntityRecognizerEntityList ::
  -- | 's3URI'
  Lude.Text ->
  EntityRecognizerEntityList
mkEntityRecognizerEntityList pS3URI_ =
  EntityRecognizerEntityList' {s3URI = pS3URI_}

-- | Specifies the Amazon S3 location where the entity list is located. The URI must be in the same region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erelS3URI :: Lens.Lens' EntityRecognizerEntityList Lude.Text
erelS3URI = Lens.lens (s3URI :: EntityRecognizerEntityList -> Lude.Text) (\s a -> s {s3URI = a} :: EntityRecognizerEntityList)
{-# DEPRECATED erelS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON EntityRecognizerEntityList where
  parseJSON =
    Lude.withObject
      "EntityRecognizerEntityList"
      (\x -> EntityRecognizerEntityList' Lude.<$> (x Lude..: "S3Uri"))

instance Lude.ToJSON EntityRecognizerEntityList where
  toJSON EntityRecognizerEntityList' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("S3Uri" Lude..= s3URI)])
