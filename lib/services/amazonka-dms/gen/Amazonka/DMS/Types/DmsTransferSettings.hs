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
-- Module      : Amazonka.DMS.Types.DmsTransferSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DmsTransferSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The settings in JSON format for the DMS Transfer type source endpoint.
--
-- /See:/ 'newDmsTransferSettings' smart constructor.
data DmsTransferSettings = DmsTransferSettings'
  { -- | The Amazon Resource Name (ARN) used by the service access IAM role. The
    -- role must allow the @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket to use.
    bucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DmsTransferSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceAccessRoleArn', 'dmsTransferSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service access IAM role. The
-- role must allow the @iam:PassRole@ action.
--
-- 'bucketName', 'dmsTransferSettings_bucketName' - The name of the S3 bucket to use.
newDmsTransferSettings ::
  DmsTransferSettings
newDmsTransferSettings =
  DmsTransferSettings'
    { serviceAccessRoleArn =
        Prelude.Nothing,
      bucketName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) used by the service access IAM role. The
-- role must allow the @iam:PassRole@ action.
dmsTransferSettings_serviceAccessRoleArn :: Lens.Lens' DmsTransferSettings (Prelude.Maybe Prelude.Text)
dmsTransferSettings_serviceAccessRoleArn = Lens.lens (\DmsTransferSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@DmsTransferSettings' {} a -> s {serviceAccessRoleArn = a} :: DmsTransferSettings)

-- | The name of the S3 bucket to use.
dmsTransferSettings_bucketName :: Lens.Lens' DmsTransferSettings (Prelude.Maybe Prelude.Text)
dmsTransferSettings_bucketName = Lens.lens (\DmsTransferSettings' {bucketName} -> bucketName) (\s@DmsTransferSettings' {} a -> s {bucketName = a} :: DmsTransferSettings)

instance Core.FromJSON DmsTransferSettings where
  parseJSON =
    Core.withObject
      "DmsTransferSettings"
      ( \x ->
          DmsTransferSettings'
            Prelude.<$> (x Core..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Core..:? "BucketName")
      )

instance Prelude.Hashable DmsTransferSettings where
  hashWithSalt _salt DmsTransferSettings' {..} =
    _salt `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData DmsTransferSettings where
  rnf DmsTransferSettings' {..} =
    Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf bucketName

instance Core.ToJSON DmsTransferSettings where
  toJSON DmsTransferSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServiceAccessRoleArn" Core..=)
              Prelude.<$> serviceAccessRoleArn,
            ("BucketName" Core..=) Prelude.<$> bucketName
          ]
      )
