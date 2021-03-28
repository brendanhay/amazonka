{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SSEKMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.SSEKMS
  ( SSEKMS (..)
  -- * Smart constructor
  , mkSSEKMS
  -- * Lenses
  , ssekmsKeyId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.SSEKMSKeyId as Types

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
-- /See:/ 'mkSSEKMS' smart constructor.
newtype SSEKMS = SSEKMS'
  { keyId :: Types.SSEKMSKeyId
    -- ^ Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SSEKMS' value with any optional fields omitted.
mkSSEKMS
    :: Types.SSEKMSKeyId -- ^ 'keyId'
    -> SSEKMS
mkSSEKMS keyId = SSEKMS'{keyId}

-- | Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssekmsKeyId :: Lens.Lens' SSEKMS Types.SSEKMSKeyId
ssekmsKeyId = Lens.field @"keyId"
{-# INLINEABLE ssekmsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.ToXML SSEKMS where
        toXML SSEKMS{..} = Core.toXMLElement "KeyId" keyId

instance Core.FromXML SSEKMS where
        parseXML x = SSEKMS' Core.<$> (x Core..@ "KeyId")
