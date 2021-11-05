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
-- Module      : Network.AWS.AppFlow.Types.UpsolverDestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.UpsolverDestinationProperties where

import Network.AWS.AppFlow.Types.UpsolverS3OutputFormatConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The properties that are applied when Upsolver is used as a destination.
--
-- /See:/ 'newUpsolverDestinationProperties' smart constructor.
data UpsolverDestinationProperties = UpsolverDestinationProperties'
  { -- | The object key for the destination Upsolver Amazon S3 bucket in which
    -- Amazon AppFlow places the files.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The Upsolver Amazon S3 bucket name in which Amazon AppFlow places the
    -- transferred data.
    bucketName :: Prelude.Text,
    -- | The configuration that determines how data is formatted when Upsolver is
    -- used as the flow destination.
    s3OutputFormatConfig :: UpsolverS3OutputFormatConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpsolverDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketPrefix', 'upsolverDestinationProperties_bucketPrefix' - The object key for the destination Upsolver Amazon S3 bucket in which
-- Amazon AppFlow places the files.
--
-- 'bucketName', 'upsolverDestinationProperties_bucketName' - The Upsolver Amazon S3 bucket name in which Amazon AppFlow places the
-- transferred data.
--
-- 's3OutputFormatConfig', 'upsolverDestinationProperties_s3OutputFormatConfig' - The configuration that determines how data is formatted when Upsolver is
-- used as the flow destination.
newUpsolverDestinationProperties ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 's3OutputFormatConfig'
  UpsolverS3OutputFormatConfig ->
  UpsolverDestinationProperties
newUpsolverDestinationProperties
  pBucketName_
  pS3OutputFormatConfig_ =
    UpsolverDestinationProperties'
      { bucketPrefix =
          Prelude.Nothing,
        bucketName = pBucketName_,
        s3OutputFormatConfig =
          pS3OutputFormatConfig_
      }

-- | The object key for the destination Upsolver Amazon S3 bucket in which
-- Amazon AppFlow places the files.
upsolverDestinationProperties_bucketPrefix :: Lens.Lens' UpsolverDestinationProperties (Prelude.Maybe Prelude.Text)
upsolverDestinationProperties_bucketPrefix = Lens.lens (\UpsolverDestinationProperties' {bucketPrefix} -> bucketPrefix) (\s@UpsolverDestinationProperties' {} a -> s {bucketPrefix = a} :: UpsolverDestinationProperties)

-- | The Upsolver Amazon S3 bucket name in which Amazon AppFlow places the
-- transferred data.
upsolverDestinationProperties_bucketName :: Lens.Lens' UpsolverDestinationProperties Prelude.Text
upsolverDestinationProperties_bucketName = Lens.lens (\UpsolverDestinationProperties' {bucketName} -> bucketName) (\s@UpsolverDestinationProperties' {} a -> s {bucketName = a} :: UpsolverDestinationProperties)

-- | The configuration that determines how data is formatted when Upsolver is
-- used as the flow destination.
upsolverDestinationProperties_s3OutputFormatConfig :: Lens.Lens' UpsolverDestinationProperties UpsolverS3OutputFormatConfig
upsolverDestinationProperties_s3OutputFormatConfig = Lens.lens (\UpsolverDestinationProperties' {s3OutputFormatConfig} -> s3OutputFormatConfig) (\s@UpsolverDestinationProperties' {} a -> s {s3OutputFormatConfig = a} :: UpsolverDestinationProperties)

instance Core.FromJSON UpsolverDestinationProperties where
  parseJSON =
    Core.withObject
      "UpsolverDestinationProperties"
      ( \x ->
          UpsolverDestinationProperties'
            Prelude.<$> (x Core..:? "bucketPrefix")
            Prelude.<*> (x Core..: "bucketName")
            Prelude.<*> (x Core..: "s3OutputFormatConfig")
      )

instance
  Prelude.Hashable
    UpsolverDestinationProperties

instance Prelude.NFData UpsolverDestinationProperties

instance Core.ToJSON UpsolverDestinationProperties where
  toJSON UpsolverDestinationProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bucketPrefix" Core..=) Prelude.<$> bucketPrefix,
            Prelude.Just ("bucketName" Core..= bucketName),
            Prelude.Just
              ( "s3OutputFormatConfig"
                  Core..= s3OutputFormatConfig
              )
          ]
      )
