{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SseKmsEncryptedObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.SseKmsEncryptedObjects
  ( SseKmsEncryptedObjects (..)
  -- * Smart constructor
  , mkSseKmsEncryptedObjects
  -- * Lenses
  , skeoStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.SseKmsEncryptedObjectsStatus as Types

-- | A container for filter information for the selection of S3 objects encrypted with AWS KMS.
--
-- /See:/ 'mkSseKmsEncryptedObjects' smart constructor.
newtype SseKmsEncryptedObjects = SseKmsEncryptedObjects'
  { status :: Types.SseKmsEncryptedObjectsStatus
    -- ^ Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SseKmsEncryptedObjects' value with any optional fields omitted.
mkSseKmsEncryptedObjects
    :: Types.SseKmsEncryptedObjectsStatus -- ^ 'status'
    -> SseKmsEncryptedObjects
mkSseKmsEncryptedObjects status = SseKmsEncryptedObjects'{status}

-- | Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skeoStatus :: Lens.Lens' SseKmsEncryptedObjects Types.SseKmsEncryptedObjectsStatus
skeoStatus = Lens.field @"status"
{-# INLINEABLE skeoStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToXML SseKmsEncryptedObjects where
        toXML SseKmsEncryptedObjects{..}
          = Core.toXMLElement "Status" status

instance Core.FromXML SseKmsEncryptedObjects where
        parseXML x = SseKmsEncryptedObjects' Core.<$> (x Core..@ "Status")
