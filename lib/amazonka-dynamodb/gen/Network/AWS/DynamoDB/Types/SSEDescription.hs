{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSEDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSEDescription
  ( SSEDescription (..),

    -- * Smart constructor
    mkSSEDescription,

    -- * Lenses
    ssedStatus,
    ssedInaccessibleEncryptionDateTime,
    ssedSSEType,
    ssedKMSMasterKeyARN,
  )
where

import Network.AWS.DynamoDB.Types.SSEStatus
import Network.AWS.DynamoDB.Types.SSEType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The description of the server-side encryption status on the specified table.
--
-- /See:/ 'mkSSEDescription' smart constructor.
data SSEDescription = SSEDescription'
  { -- | Represents the current state of server-side encryption. The only supported values are:
    --
    --
    --     * @ENABLED@ - Server-side encryption is enabled.
    --
    --
    --     * @UPDATING@ - Server-side encryption is being updated.
    status :: Lude.Maybe SSEStatus,
    -- | Indicates the time, in UNIX epoch date format, when DynamoDB detected that the table's AWS KMS key was inaccessible. This attribute will automatically be cleared when DynamoDB detects that the table's AWS KMS key is accessible again. DynamoDB will initiate the table archival process when table's AWS KMS key remains inaccessible for more than seven days from this date.
    inaccessibleEncryptionDateTime :: Lude.Maybe Lude.Timestamp,
    -- | Server-side encryption type. The only supported value is:
    --
    --
    --     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
    sSEType :: Lude.Maybe SSEType,
    -- | The AWS KMS customer master key (CMK) ARN used for the AWS KMS encryption.
    kmsMasterKeyARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSEDescription' with the minimum fields required to make a request.
--
-- * 'status' - Represents the current state of server-side encryption. The only supported values are:
--
--
--     * @ENABLED@ - Server-side encryption is enabled.
--
--
--     * @UPDATING@ - Server-side encryption is being updated.
--
--
-- * 'inaccessibleEncryptionDateTime' - Indicates the time, in UNIX epoch date format, when DynamoDB detected that the table's AWS KMS key was inaccessible. This attribute will automatically be cleared when DynamoDB detects that the table's AWS KMS key is accessible again. DynamoDB will initiate the table archival process when table's AWS KMS key remains inaccessible for more than seven days from this date.
-- * 'sSEType' - Server-side encryption type. The only supported value is:
--
--
--     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
--
--
-- * 'kmsMasterKeyARN' - The AWS KMS customer master key (CMK) ARN used for the AWS KMS encryption.
mkSSEDescription ::
  SSEDescription
mkSSEDescription =
  SSEDescription'
    { status = Lude.Nothing,
      inaccessibleEncryptionDateTime = Lude.Nothing,
      sSEType = Lude.Nothing,
      kmsMasterKeyARN = Lude.Nothing
    }

-- | Represents the current state of server-side encryption. The only supported values are:
--
--
--     * @ENABLED@ - Server-side encryption is enabled.
--
--
--     * @UPDATING@ - Server-side encryption is being updated.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedStatus :: Lens.Lens' SSEDescription (Lude.Maybe SSEStatus)
ssedStatus = Lens.lens (status :: SSEDescription -> Lude.Maybe SSEStatus) (\s a -> s {status = a} :: SSEDescription)
{-# DEPRECATED ssedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates the time, in UNIX epoch date format, when DynamoDB detected that the table's AWS KMS key was inaccessible. This attribute will automatically be cleared when DynamoDB detects that the table's AWS KMS key is accessible again. DynamoDB will initiate the table archival process when table's AWS KMS key remains inaccessible for more than seven days from this date.
--
-- /Note:/ Consider using 'inaccessibleEncryptionDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedInaccessibleEncryptionDateTime :: Lens.Lens' SSEDescription (Lude.Maybe Lude.Timestamp)
ssedInaccessibleEncryptionDateTime = Lens.lens (inaccessibleEncryptionDateTime :: SSEDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {inaccessibleEncryptionDateTime = a} :: SSEDescription)
{-# DEPRECATED ssedInaccessibleEncryptionDateTime "Use generic-lens or generic-optics with 'inaccessibleEncryptionDateTime' instead." #-}

-- | Server-side encryption type. The only supported value is:
--
--
--     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
--
--
--
-- /Note:/ Consider using 'sSEType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedSSEType :: Lens.Lens' SSEDescription (Lude.Maybe SSEType)
ssedSSEType = Lens.lens (sSEType :: SSEDescription -> Lude.Maybe SSEType) (\s a -> s {sSEType = a} :: SSEDescription)
{-# DEPRECATED ssedSSEType "Use generic-lens or generic-optics with 'sSEType' instead." #-}

-- | The AWS KMS customer master key (CMK) ARN used for the AWS KMS encryption.
--
-- /Note:/ Consider using 'kmsMasterKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedKMSMasterKeyARN :: Lens.Lens' SSEDescription (Lude.Maybe Lude.Text)
ssedKMSMasterKeyARN = Lens.lens (kmsMasterKeyARN :: SSEDescription -> Lude.Maybe Lude.Text) (\s a -> s {kmsMasterKeyARN = a} :: SSEDescription)
{-# DEPRECATED ssedKMSMasterKeyARN "Use generic-lens or generic-optics with 'kmsMasterKeyARN' instead." #-}

instance Lude.FromJSON SSEDescription where
  parseJSON =
    Lude.withObject
      "SSEDescription"
      ( \x ->
          SSEDescription'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "InaccessibleEncryptionDateTime")
            Lude.<*> (x Lude..:? "SSEType")
            Lude.<*> (x Lude..:? "KMSMasterKeyArn")
      )
