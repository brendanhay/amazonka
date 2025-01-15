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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DmsTransferSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings in JSON format for the DMS Transfer type source endpoint.
--
-- /See:/ 'newDmsTransferSettings' smart constructor.
data DmsTransferSettings = DmsTransferSettings'
  { -- | The name of the S3 bucket to use.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) used by the service access IAM role. The
    -- role must allow the @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text
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
-- 'bucketName', 'dmsTransferSettings_bucketName' - The name of the S3 bucket to use.
--
-- 'serviceAccessRoleArn', 'dmsTransferSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service access IAM role. The
-- role must allow the @iam:PassRole@ action.
newDmsTransferSettings ::
  DmsTransferSettings
newDmsTransferSettings =
  DmsTransferSettings'
    { bucketName = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing
    }

-- | The name of the S3 bucket to use.
dmsTransferSettings_bucketName :: Lens.Lens' DmsTransferSettings (Prelude.Maybe Prelude.Text)
dmsTransferSettings_bucketName = Lens.lens (\DmsTransferSettings' {bucketName} -> bucketName) (\s@DmsTransferSettings' {} a -> s {bucketName = a} :: DmsTransferSettings)

-- | The Amazon Resource Name (ARN) used by the service access IAM role. The
-- role must allow the @iam:PassRole@ action.
dmsTransferSettings_serviceAccessRoleArn :: Lens.Lens' DmsTransferSettings (Prelude.Maybe Prelude.Text)
dmsTransferSettings_serviceAccessRoleArn = Lens.lens (\DmsTransferSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@DmsTransferSettings' {} a -> s {serviceAccessRoleArn = a} :: DmsTransferSettings)

instance Data.FromJSON DmsTransferSettings where
  parseJSON =
    Data.withObject
      "DmsTransferSettings"
      ( \x ->
          DmsTransferSettings'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "ServiceAccessRoleArn")
      )

instance Prelude.Hashable DmsTransferSettings where
  hashWithSalt _salt DmsTransferSettings' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` serviceAccessRoleArn

instance Prelude.NFData DmsTransferSettings where
  rnf DmsTransferSettings' {..} =
    Prelude.rnf bucketName `Prelude.seq`
      Prelude.rnf serviceAccessRoleArn

instance Data.ToJSON DmsTransferSettings where
  toJSON DmsTransferSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("ServiceAccessRoleArn" Data..=)
              Prelude.<$> serviceAccessRoleArn
          ]
      )
