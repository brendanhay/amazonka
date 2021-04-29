{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.S3DestinationAccessControl
import Network.AWS.MediaConvert.Types.S3EncryptionSettings
import qualified Network.AWS.Prelude as Prelude

-- | Settings associated with S3 destination
--
-- /See:/ 'newS3DestinationSettings' smart constructor.
data S3DestinationSettings = S3DestinationSettings'
  { -- | Settings for how your job outputs are encrypted as they are uploaded to
    -- Amazon S3.
    encryption :: Prelude.Maybe S3EncryptionSettings,
    -- | Optional. Have MediaConvert automatically apply Amazon S3 access control
    -- for the outputs in this output group. When you don\'t use this setting,
    -- S3 automatically applies the default access control list PRIVATE.
    accessControl :: Prelude.Maybe S3DestinationAccessControl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 's3DestinationSettings_encryption' - Settings for how your job outputs are encrypted as they are uploaded to
-- Amazon S3.
--
-- 'accessControl', 's3DestinationSettings_accessControl' - Optional. Have MediaConvert automatically apply Amazon S3 access control
-- for the outputs in this output group. When you don\'t use this setting,
-- S3 automatically applies the default access control list PRIVATE.
newS3DestinationSettings ::
  S3DestinationSettings
newS3DestinationSettings =
  S3DestinationSettings'
    { encryption =
        Prelude.Nothing,
      accessControl = Prelude.Nothing
    }

-- | Settings for how your job outputs are encrypted as they are uploaded to
-- Amazon S3.
s3DestinationSettings_encryption :: Lens.Lens' S3DestinationSettings (Prelude.Maybe S3EncryptionSettings)
s3DestinationSettings_encryption = Lens.lens (\S3DestinationSettings' {encryption} -> encryption) (\s@S3DestinationSettings' {} a -> s {encryption = a} :: S3DestinationSettings)

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control
-- for the outputs in this output group. When you don\'t use this setting,
-- S3 automatically applies the default access control list PRIVATE.
s3DestinationSettings_accessControl :: Lens.Lens' S3DestinationSettings (Prelude.Maybe S3DestinationAccessControl)
s3DestinationSettings_accessControl = Lens.lens (\S3DestinationSettings' {accessControl} -> accessControl) (\s@S3DestinationSettings' {} a -> s {accessControl = a} :: S3DestinationSettings)

instance Prelude.FromJSON S3DestinationSettings where
  parseJSON =
    Prelude.withObject
      "S3DestinationSettings"
      ( \x ->
          S3DestinationSettings'
            Prelude.<$> (x Prelude..:? "encryption")
            Prelude.<*> (x Prelude..:? "accessControl")
      )

instance Prelude.Hashable S3DestinationSettings

instance Prelude.NFData S3DestinationSettings

instance Prelude.ToJSON S3DestinationSettings where
  toJSON S3DestinationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("encryption" Prelude..=) Prelude.<$> encryption,
            ("accessControl" Prelude..=)
              Prelude.<$> accessControl
          ]
      )
