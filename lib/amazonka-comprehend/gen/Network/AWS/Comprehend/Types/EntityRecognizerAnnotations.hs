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
    eraS3Uri,
  )
where

import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the annotations associated with a entity recognizer.
--
-- /See:/ 'mkEntityRecognizerAnnotations' smart constructor.
newtype EntityRecognizerAnnotations = EntityRecognizerAnnotations'
  { -- | Specifies the Amazon S3 location where the annotations for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
    s3Uri :: Types.S3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EntityRecognizerAnnotations' value with any optional fields omitted.
mkEntityRecognizerAnnotations ::
  -- | 's3Uri'
  Types.S3Uri ->
  EntityRecognizerAnnotations
mkEntityRecognizerAnnotations s3Uri =
  EntityRecognizerAnnotations' {s3Uri}

-- | Specifies the Amazon S3 location where the annotations for an entity recognizer are located. The URI must be in the same region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eraS3Uri :: Lens.Lens' EntityRecognizerAnnotations Types.S3Uri
eraS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED eraS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

instance Core.FromJSON EntityRecognizerAnnotations where
  toJSON EntityRecognizerAnnotations {..} =
    Core.object (Core.catMaybes [Core.Just ("S3Uri" Core..= s3Uri)])

instance Core.FromJSON EntityRecognizerAnnotations where
  parseJSON =
    Core.withObject "EntityRecognizerAnnotations" Core.$
      \x -> EntityRecognizerAnnotations' Core.<$> (x Core..: "S3Uri")
