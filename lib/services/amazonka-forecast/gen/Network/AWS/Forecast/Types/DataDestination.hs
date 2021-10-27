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
-- Module      : Network.AWS.Forecast.Types.DataDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Forecast.Types.DataDestination where

import qualified Network.AWS.Core as Core
import Network.AWS.Forecast.Types.S3Config
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The destination for an export job. Provide an S3 path, an AWS Identity
-- and Access Management (IAM) role that allows Amazon Forecast to access
-- the location, and an AWS Key Management Service (KMS) key (optional).
--
-- /See:/ 'newDataDestination' smart constructor.
data DataDestination = DataDestination'
  { -- | The path to an Amazon Simple Storage Service (Amazon S3) bucket along
    -- with the credentials to access the bucket.
    s3Config :: S3Config
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Config', 'dataDestination_s3Config' - The path to an Amazon Simple Storage Service (Amazon S3) bucket along
-- with the credentials to access the bucket.
newDataDestination ::
  -- | 's3Config'
  S3Config ->
  DataDestination
newDataDestination pS3Config_ =
  DataDestination' {s3Config = pS3Config_}

-- | The path to an Amazon Simple Storage Service (Amazon S3) bucket along
-- with the credentials to access the bucket.
dataDestination_s3Config :: Lens.Lens' DataDestination S3Config
dataDestination_s3Config = Lens.lens (\DataDestination' {s3Config} -> s3Config) (\s@DataDestination' {} a -> s {s3Config = a} :: DataDestination)

instance Core.FromJSON DataDestination where
  parseJSON =
    Core.withObject
      "DataDestination"
      ( \x ->
          DataDestination' Prelude.<$> (x Core..: "S3Config")
      )

instance Prelude.Hashable DataDestination

instance Prelude.NFData DataDestination

instance Core.ToJSON DataDestination where
  toJSON DataDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Config" Core..= s3Config)]
      )
