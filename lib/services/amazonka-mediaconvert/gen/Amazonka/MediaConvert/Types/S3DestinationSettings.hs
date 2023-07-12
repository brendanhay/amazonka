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
-- Module      : Amazonka.MediaConvert.Types.S3DestinationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.S3DestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.S3DestinationAccessControl
import Amazonka.MediaConvert.Types.S3EncryptionSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings associated with S3 destination
--
-- /See:/ 'newS3DestinationSettings' smart constructor.
data S3DestinationSettings = S3DestinationSettings'
  { -- | Optional. Have MediaConvert automatically apply Amazon S3 access control
    -- for the outputs in this output group. When you don\'t use this setting,
    -- S3 automatically applies the default access control list PRIVATE.
    accessControl :: Prelude.Maybe S3DestinationAccessControl,
    -- | Settings for how your job outputs are encrypted as they are uploaded to
    -- Amazon S3.
    encryption :: Prelude.Maybe S3EncryptionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControl', 's3DestinationSettings_accessControl' - Optional. Have MediaConvert automatically apply Amazon S3 access control
-- for the outputs in this output group. When you don\'t use this setting,
-- S3 automatically applies the default access control list PRIVATE.
--
-- 'encryption', 's3DestinationSettings_encryption' - Settings for how your job outputs are encrypted as they are uploaded to
-- Amazon S3.
newS3DestinationSettings ::
  S3DestinationSettings
newS3DestinationSettings =
  S3DestinationSettings'
    { accessControl =
        Prelude.Nothing,
      encryption = Prelude.Nothing
    }

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control
-- for the outputs in this output group. When you don\'t use this setting,
-- S3 automatically applies the default access control list PRIVATE.
s3DestinationSettings_accessControl :: Lens.Lens' S3DestinationSettings (Prelude.Maybe S3DestinationAccessControl)
s3DestinationSettings_accessControl = Lens.lens (\S3DestinationSettings' {accessControl} -> accessControl) (\s@S3DestinationSettings' {} a -> s {accessControl = a} :: S3DestinationSettings)

-- | Settings for how your job outputs are encrypted as they are uploaded to
-- Amazon S3.
s3DestinationSettings_encryption :: Lens.Lens' S3DestinationSettings (Prelude.Maybe S3EncryptionSettings)
s3DestinationSettings_encryption = Lens.lens (\S3DestinationSettings' {encryption} -> encryption) (\s@S3DestinationSettings' {} a -> s {encryption = a} :: S3DestinationSettings)

instance Data.FromJSON S3DestinationSettings where
  parseJSON =
    Data.withObject
      "S3DestinationSettings"
      ( \x ->
          S3DestinationSettings'
            Prelude.<$> (x Data..:? "accessControl")
            Prelude.<*> (x Data..:? "encryption")
      )

instance Prelude.Hashable S3DestinationSettings where
  hashWithSalt _salt S3DestinationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` accessControl
      `Prelude.hashWithSalt` encryption

instance Prelude.NFData S3DestinationSettings where
  rnf S3DestinationSettings' {..} =
    Prelude.rnf accessControl
      `Prelude.seq` Prelude.rnf encryption

instance Data.ToJSON S3DestinationSettings where
  toJSON S3DestinationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessControl" Data..=) Prelude.<$> accessControl,
            ("encryption" Data..=) Prelude.<$> encryption
          ]
      )
