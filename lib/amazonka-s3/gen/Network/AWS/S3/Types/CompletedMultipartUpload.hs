{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CompletedMultipartUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.CompletedMultipartUpload
  ( CompletedMultipartUpload (..)
  -- * Smart constructor
  , mkCompletedMultipartUpload
  -- * Lenses
  , cmuParts
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.CompletedPart as Types

-- | The container for the completed multipart upload details.
--
-- /See:/ 'mkCompletedMultipartUpload' smart constructor.
newtype CompletedMultipartUpload = CompletedMultipartUpload'
  { parts :: Core.Maybe (Core.NonEmpty Types.CompletedPart)
    -- ^ Array of CompletedPart data types.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CompletedMultipartUpload' value with any optional fields omitted.
mkCompletedMultipartUpload
    :: CompletedMultipartUpload
mkCompletedMultipartUpload
  = CompletedMultipartUpload'{parts = Core.Nothing}

-- | Array of CompletedPart data types.
--
-- /Note:/ Consider using 'parts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmuParts :: Lens.Lens' CompletedMultipartUpload (Core.Maybe (Core.NonEmpty Types.CompletedPart))
cmuParts = Lens.field @"parts"
{-# INLINEABLE cmuParts #-}
{-# DEPRECATED parts "Use generic-lens or generic-optics with 'parts' instead"  #-}

instance Core.ToXML CompletedMultipartUpload where
        toXML CompletedMultipartUpload{..}
          = Core.maybe Core.mempty (Core.toXMLList "Part") parts
