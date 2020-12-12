{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.EncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.EncryptionConfig
  ( EncryptionConfig (..),

    -- * Smart constructor
    mkEncryptionConfig,

    -- * Lenses
    ecStatus,
    ecKeyId,
    ecType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.EncryptionStatus
import Network.AWS.XRay.Types.EncryptionType

-- | A configuration document that specifies encryption configuration settings.
--
-- /See:/ 'mkEncryptionConfig' smart constructor.
data EncryptionConfig = EncryptionConfig'
  { status ::
      Lude.Maybe EncryptionStatus,
    keyId :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe EncryptionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionConfig' with the minimum fields required to make a request.
--
-- * 'keyId' - The ID of the customer master key (CMK) used for encryption, if applicable.
-- * 'status' - The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
-- * 'type'' - The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
mkEncryptionConfig ::
  EncryptionConfig
mkEncryptionConfig =
  EncryptionConfig'
    { status = Lude.Nothing,
      keyId = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The encryption status. While the status is @UPDATING@ , X-Ray may encrypt data with a combination of the new and old settings.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecStatus :: Lens.Lens' EncryptionConfig (Lude.Maybe EncryptionStatus)
ecStatus = Lens.lens (status :: EncryptionConfig -> Lude.Maybe EncryptionStatus) (\s a -> s {status = a} :: EncryptionConfig)
{-# DEPRECATED ecStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the customer master key (CMK) used for encryption, if applicable.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKeyId :: Lens.Lens' EncryptionConfig (Lude.Maybe Lude.Text)
ecKeyId = Lens.lens (keyId :: EncryptionConfig -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: EncryptionConfig)
{-# DEPRECATED ecKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The type of encryption. Set to @KMS@ for encryption with CMKs. Set to @NONE@ for default encryption.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecType :: Lens.Lens' EncryptionConfig (Lude.Maybe EncryptionType)
ecType = Lens.lens (type' :: EncryptionConfig -> Lude.Maybe EncryptionType) (\s a -> s {type' = a} :: EncryptionConfig)
{-# DEPRECATED ecType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON EncryptionConfig where
  parseJSON =
    Lude.withObject
      "EncryptionConfig"
      ( \x ->
          EncryptionConfig'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "KeyId")
            Lude.<*> (x Lude..:? "Type")
      )
