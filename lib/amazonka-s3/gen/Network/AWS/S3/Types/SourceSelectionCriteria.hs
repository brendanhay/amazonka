-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SourceSelectionCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SourceSelectionCriteria
  ( SourceSelectionCriteria (..),

    -- * Smart constructor
    mkSourceSelectionCriteria,

    -- * Lenses
    sscSseKMSEncryptedObjects,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.SseKMSEncryptedObjects

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
-- /See:/ 'mkSourceSelectionCriteria' smart constructor.
newtype SourceSelectionCriteria = SourceSelectionCriteria'
  { sseKMSEncryptedObjects ::
      Lude.Maybe SseKMSEncryptedObjects
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceSelectionCriteria' with the minimum fields required to make a request.
--
-- * 'sseKMSEncryptedObjects' - A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required.
mkSourceSelectionCriteria ::
  SourceSelectionCriteria
mkSourceSelectionCriteria =
  SourceSelectionCriteria' {sseKMSEncryptedObjects = Lude.Nothing}

-- | A container for filter information for the selection of Amazon S3 objects encrypted with AWS KMS. If you include @SourceSelectionCriteria@ in the replication configuration, this element is required.
--
-- /Note:/ Consider using 'sseKMSEncryptedObjects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscSseKMSEncryptedObjects :: Lens.Lens' SourceSelectionCriteria (Lude.Maybe SseKMSEncryptedObjects)
sscSseKMSEncryptedObjects = Lens.lens (sseKMSEncryptedObjects :: SourceSelectionCriteria -> Lude.Maybe SseKMSEncryptedObjects) (\s a -> s {sseKMSEncryptedObjects = a} :: SourceSelectionCriteria)
{-# DEPRECATED sscSseKMSEncryptedObjects "Use generic-lens or generic-optics with 'sseKMSEncryptedObjects' instead." #-}

instance Lude.FromXML SourceSelectionCriteria where
  parseXML x =
    SourceSelectionCriteria'
      Lude.<$> (x Lude..@? "SseKmsEncryptedObjects")

instance Lude.ToXML SourceSelectionCriteria where
  toXML SourceSelectionCriteria' {..} =
    Lude.mconcat
      ["SseKmsEncryptedObjects" Lude.@= sseKMSEncryptedObjects]
