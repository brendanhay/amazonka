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
    ssekKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
-- /See:/ 'mkSSEKMS' smart constructor.
newtype SSEKMS = SSEKMS'
  { -- | Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
    keyId :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSEKMS' with the minimum fields required to make a request.
--
-- * 'keyId' - Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
mkSSEKMS ::
  -- | 'keyId'
  Lude.Sensitive Lude.Text ->
  SSEKMS
mkSSEKMS pKeyId_ = SSEKMS' {keyId = pKeyId_}

-- | Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssekKeyId :: Lens.Lens' SSEKMS (Lude.Sensitive Lude.Text)
ssekKeyId = Lens.lens (keyId :: SSEKMS -> Lude.Sensitive Lude.Text) (\s a -> s {keyId = a} :: SSEKMS)
{-# DEPRECATED ssekKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Lude.FromXML SSEKMS where
  parseXML x = SSEKMS' Lude.<$> (x Lude..@ "KeyId")

instance Lude.ToXML SSEKMS where
  toXML SSEKMS' {..} = Lude.mconcat ["KeyId" Lude.@= keyId]
