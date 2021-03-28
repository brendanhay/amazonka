{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EntityRecognizerDocuments
  ( EntityRecognizerDocuments (..)
  -- * Smart constructor
  , mkEntityRecognizerDocuments
  -- * Lenses
  , erdS3Uri
  ) where

import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the training documents submitted with an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerDocuments' smart constructor.
newtype EntityRecognizerDocuments = EntityRecognizerDocuments'
  { s3Uri :: Types.S3Uri
    -- ^ Specifies the Amazon S3 location where the training documents for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EntityRecognizerDocuments' value with any optional fields omitted.
mkEntityRecognizerDocuments
    :: Types.S3Uri -- ^ 's3Uri'
    -> EntityRecognizerDocuments
mkEntityRecognizerDocuments s3Uri
  = EntityRecognizerDocuments'{s3Uri}

-- | Specifies the Amazon S3 location where the training documents for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erdS3Uri :: Lens.Lens' EntityRecognizerDocuments Types.S3Uri
erdS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE erdS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

instance Core.FromJSON EntityRecognizerDocuments where
        toJSON EntityRecognizerDocuments{..}
          = Core.object (Core.catMaybes [Core.Just ("S3Uri" Core..= s3Uri)])

instance Core.FromJSON EntityRecognizerDocuments where
        parseJSON
          = Core.withObject "EntityRecognizerDocuments" Core.$
              \ x -> EntityRecognizerDocuments' Core.<$> (x Core..: "S3Uri")
