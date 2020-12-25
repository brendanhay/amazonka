{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SSEKMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SSEKMS
  ( SSEKMS (..),

    -- * Smart constructor
    mkSSEKMS,

    -- * Lenses
    ssekmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.SSEKMSKeyId as Types

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
-- /See:/ 'mkSSEKMS' smart constructor.
newtype SSEKMS = SSEKMS'
  { -- | Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
    keyId :: Types.SSEKMSKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SSEKMS' value with any optional fields omitted.
mkSSEKMS ::
  -- | 'keyId'
  Types.SSEKMSKeyId ->
  SSEKMS
mkSSEKMS keyId = SSEKMS' {keyId}

-- | Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssekmsKeyId :: Lens.Lens' SSEKMS Types.SSEKMSKeyId
ssekmsKeyId = Lens.field @"keyId"
{-# DEPRECATED ssekmsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Core.ToXML SSEKMS where
  toXML SSEKMS {..} = Core.toXMLNode "KeyId" keyId

instance Core.FromXML SSEKMS where
  parseXML x = SSEKMS' Core.<$> (x Core..@ "KeyId")
