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
-- Module      : Network.AWS.DMS.Types.DmsTransferSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DmsTransferSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The settings in JSON format for the DMS Transfer type source endpoint.
--
-- /See:/ 'newDmsTransferSettings' smart constructor.
data DmsTransferSettings = DmsTransferSettings'
  { -- | The name of the S3 bucket to use.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that has permission to access the Amazon S3 bucket.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'serviceAccessRoleArn', 'dmsTransferSettings_serviceAccessRoleArn' - The IAM role that has permission to access the Amazon S3 bucket.
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

-- | The IAM role that has permission to access the Amazon S3 bucket.
dmsTransferSettings_serviceAccessRoleArn :: Lens.Lens' DmsTransferSettings (Prelude.Maybe Prelude.Text)
dmsTransferSettings_serviceAccessRoleArn = Lens.lens (\DmsTransferSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@DmsTransferSettings' {} a -> s {serviceAccessRoleArn = a} :: DmsTransferSettings)

instance Prelude.FromJSON DmsTransferSettings where
  parseJSON =
    Prelude.withObject
      "DmsTransferSettings"
      ( \x ->
          DmsTransferSettings'
            Prelude.<$> (x Prelude..:? "BucketName")
            Prelude.<*> (x Prelude..:? "ServiceAccessRoleArn")
      )

instance Prelude.Hashable DmsTransferSettings

instance Prelude.NFData DmsTransferSettings

instance Prelude.ToJSON DmsTransferSettings where
  toJSON DmsTransferSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("BucketName" Prelude..=) Prelude.<$> bucketName,
            ("ServiceAccessRoleArn" Prelude..=)
              Prelude.<$> serviceAccessRoleArn
          ]
      )
