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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.S3ContentBaseLocation
import qualified Amazonka.Prelude as Prelude

-- | The information required to deploy a Kinesis Data Analytics Studio
-- notebook as an application with durable state.
--
-- /See:/ 'newDeployAsApplicationConfiguration' smart constructor.
data DeployAsApplicationConfiguration = DeployAsApplicationConfiguration'
  { -- | The description of an Amazon S3 object that contains the Amazon Data
    -- Analytics application, including the Amazon Resource Name (ARN) of the
    -- S3 bucket, the name of the Amazon S3 object that contains the data, and
    -- the version number of the Amazon S3 object that contains the data.
    s3ContentLocation :: S3ContentBaseLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeployAsApplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ContentLocation', 'deployAsApplicationConfiguration_s3ContentLocation' - The description of an Amazon S3 object that contains the Amazon Data
-- Analytics application, including the Amazon Resource Name (ARN) of the
-- S3 bucket, the name of the Amazon S3 object that contains the data, and
-- the version number of the Amazon S3 object that contains the data.
newDeployAsApplicationConfiguration ::
  -- | 's3ContentLocation'
  S3ContentBaseLocation ->
  DeployAsApplicationConfiguration
newDeployAsApplicationConfiguration
  pS3ContentLocation_ =
    DeployAsApplicationConfiguration'
      { s3ContentLocation =
          pS3ContentLocation_
      }

-- | The description of an Amazon S3 object that contains the Amazon Data
-- Analytics application, including the Amazon Resource Name (ARN) of the
-- S3 bucket, the name of the Amazon S3 object that contains the data, and
-- the version number of the Amazon S3 object that contains the data.
deployAsApplicationConfiguration_s3ContentLocation :: Lens.Lens' DeployAsApplicationConfiguration S3ContentBaseLocation
deployAsApplicationConfiguration_s3ContentLocation = Lens.lens (\DeployAsApplicationConfiguration' {s3ContentLocation} -> s3ContentLocation) (\s@DeployAsApplicationConfiguration' {} a -> s {s3ContentLocation = a} :: DeployAsApplicationConfiguration)

instance
  Prelude.Hashable
    DeployAsApplicationConfiguration
  where
  hashWithSalt
    _salt
    DeployAsApplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` s3ContentLocation

instance
  Prelude.NFData
    DeployAsApplicationConfiguration
  where
  rnf DeployAsApplicationConfiguration' {..} =
    Prelude.rnf s3ContentLocation

instance Core.ToJSON DeployAsApplicationConfiguration where
  toJSON DeployAsApplicationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("S3ContentLocation" Core..= s3ContentLocation)
          ]
      )
