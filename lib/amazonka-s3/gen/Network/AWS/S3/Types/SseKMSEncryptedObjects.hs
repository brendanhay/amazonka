{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SseKMSEncryptedObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SseKMSEncryptedObjects
  ( SseKMSEncryptedObjects (..),

    -- * Smart constructor
    mkSseKMSEncryptedObjects,

    -- * Lenses
    skeoStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.SseKMSEncryptedObjectsStatus

-- | A container for filter information for the selection of S3 objects encrypted with AWS KMS.
--
-- /See:/ 'mkSseKMSEncryptedObjects' smart constructor.
newtype SseKMSEncryptedObjects = SseKMSEncryptedObjects'
  { status ::
      SseKMSEncryptedObjectsStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SseKMSEncryptedObjects' with the minimum fields required to make a request.
--
-- * 'status' - Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
mkSseKMSEncryptedObjects ::
  -- | 'status'
  SseKMSEncryptedObjectsStatus ->
  SseKMSEncryptedObjects
mkSseKMSEncryptedObjects pStatus_ =
  SseKMSEncryptedObjects' {status = pStatus_}

-- | Specifies whether Amazon S3 replicates objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skeoStatus :: Lens.Lens' SseKMSEncryptedObjects SseKMSEncryptedObjectsStatus
skeoStatus = Lens.lens (status :: SseKMSEncryptedObjects -> SseKMSEncryptedObjectsStatus) (\s a -> s {status = a} :: SseKMSEncryptedObjects)
{-# DEPRECATED skeoStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML SseKMSEncryptedObjects where
  parseXML x = SseKMSEncryptedObjects' Lude.<$> (x Lude..@ "Status")

instance Lude.ToXML SseKMSEncryptedObjects where
  toXML SseKMSEncryptedObjects' {..} =
    Lude.mconcat ["Status" Lude.@= status]
