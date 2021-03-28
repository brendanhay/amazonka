{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SourceSelectionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.SourceSelectionCriteria
  ( SourceSelectionCriteria (..)
  -- * Smart constructor
  , mkSourceSelectionCriteria
  -- * Lenses
  , sscSseKmsEncryptedObjects
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.SseKmsEncryptedObjects as Types

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
-- /See:/ 'mkSourceSelectionCriteria' smart constructor.
newtype SourceSelectionCriteria = SourceSelectionCriteria'
  { sseKmsEncryptedObjects :: Core.Maybe Types.SseKmsEncryptedObjects
    -- ^ A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SourceSelectionCriteria' value with any optional fields omitted.
mkSourceSelectionCriteria
    :: SourceSelectionCriteria
mkSourceSelectionCriteria
  = SourceSelectionCriteria'{sseKmsEncryptedObjects = Core.Nothing}

-- | A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required. 
--
-- /Note:/ Consider using 'sseKmsEncryptedObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscSseKmsEncryptedObjects :: Lens.Lens' SourceSelectionCriteria (Core.Maybe Types.SseKmsEncryptedObjects)
sscSseKmsEncryptedObjects = Lens.field @"sseKmsEncryptedObjects"
{-# INLINEABLE sscSseKmsEncryptedObjects #-}
{-# DEPRECATED sseKmsEncryptedObjects "Use generic-lens or generic-optics with 'sseKmsEncryptedObjects' instead"  #-}

instance Core.ToXML SourceSelectionCriteria where
        toXML SourceSelectionCriteria{..}
          = Core.maybe Core.mempty
              (Core.toXMLElement "SseKmsEncryptedObjects")
              sseKmsEncryptedObjects

instance Core.FromXML SourceSelectionCriteria where
        parseXML x
          = SourceSelectionCriteria' Core.<$>
              (x Core..@? "SseKmsEncryptedObjects")
