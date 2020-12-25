{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    erelS3Uri,
  )
where

import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the entity recognizer submitted with an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerEntityList' smart constructor.
newtype EntityRecognizerEntityList = EntityRecognizerEntityList'
  { -- | Specifies the Amazon S3 location where the entity list is located. The URI must be in the same region as the API endpoint that you are calling.
    s3Uri :: Types.S3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EntityRecognizerEntityList' value with any optional fields omitted.
mkEntityRecognizerEntityList ::
  -- | 's3Uri'
  Types.S3Uri ->
  EntityRecognizerEntityList
mkEntityRecognizerEntityList s3Uri =
  EntityRecognizerEntityList' {s3Uri}

-- | Specifies the Amazon S3 location where the entity list is located. The URI must be in the same region as the API endpoint that you are calling.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erelS3Uri :: Lens.Lens' EntityRecognizerEntityList Types.S3Uri
erelS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED erelS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

instance Core.FromJSON EntityRecognizerEntityList where
  toJSON EntityRecognizerEntityList {..} =
    Core.object (Core.catMaybes [Core.Just ("S3Uri" Core..= s3Uri)])

instance Core.FromJSON EntityRecognizerEntityList where
  parseJSON =
    Core.withObject "EntityRecognizerEntityList" Core.$
      \x -> EntityRecognizerEntityList' Core.<$> (x Core..: "S3Uri")
