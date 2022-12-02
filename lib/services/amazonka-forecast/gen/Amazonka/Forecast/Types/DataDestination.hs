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
-- Module      : Amazonka.Forecast.Types.DataDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.DataDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.S3Config
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON DataDestination where
  parseJSON =
    Data.withObject
      "DataDestination"
      ( \x ->
          DataDestination' Prelude.<$> (x Data..: "S3Config")
      )

instance Prelude.Hashable DataDestination where
  hashWithSalt _salt DataDestination' {..} =
    _salt `Prelude.hashWithSalt` s3Config

instance Prelude.NFData DataDestination where
  rnf DataDestination' {..} = Prelude.rnf s3Config

instance Data.ToJSON DataDestination where
  toJSON DataDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Config" Data..= s3Config)]
      )
