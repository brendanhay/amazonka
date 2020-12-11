-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationSettings
  ( S3DestinationSettings (..),

    -- * Smart constructor
    mkS3DestinationSettings,

    -- * Lenses
    sdsAccessControl,
    sdsEncryption,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3DestinationAccessControl
import Network.AWS.MediaConvert.Types.S3EncryptionSettings
import qualified Network.AWS.Prelude as Lude

-- | Settings associated with S3 destination
--
-- /See:/ 'mkS3DestinationSettings' smart constructor.
data S3DestinationSettings = S3DestinationSettings'
  { accessControl ::
      Lude.Maybe S3DestinationAccessControl,
    encryption :: Lude.Maybe S3EncryptionSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3DestinationSettings' with the minimum fields required to make a request.
--
-- * 'accessControl' - Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
-- * 'encryption' - Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
mkS3DestinationSettings ::
  S3DestinationSettings
mkS3DestinationSettings =
  S3DestinationSettings'
    { accessControl = Lude.Nothing,
      encryption = Lude.Nothing
    }

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
--
-- /Note:/ Consider using 'accessControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsAccessControl :: Lens.Lens' S3DestinationSettings (Lude.Maybe S3DestinationAccessControl)
sdsAccessControl = Lens.lens (accessControl :: S3DestinationSettings -> Lude.Maybe S3DestinationAccessControl) (\s a -> s {accessControl = a} :: S3DestinationSettings)
{-# DEPRECATED sdsAccessControl "Use generic-lens or generic-optics with 'accessControl' instead." #-}

-- | Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsEncryption :: Lens.Lens' S3DestinationSettings (Lude.Maybe S3EncryptionSettings)
sdsEncryption = Lens.lens (encryption :: S3DestinationSettings -> Lude.Maybe S3EncryptionSettings) (\s a -> s {encryption = a} :: S3DestinationSettings)
{-# DEPRECATED sdsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

instance Lude.FromJSON S3DestinationSettings where
  parseJSON =
    Lude.withObject
      "S3DestinationSettings"
      ( \x ->
          S3DestinationSettings'
            Lude.<$> (x Lude..:? "accessControl") Lude.<*> (x Lude..:? "encryption")
      )

instance Lude.ToJSON S3DestinationSettings where
  toJSON S3DestinationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("accessControl" Lude..=) Lude.<$> accessControl,
            ("encryption" Lude..=) Lude.<$> encryption
          ]
      )
