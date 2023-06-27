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
-- Module      : Amazonka.SageMaker.Types.HubS3StorageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HubS3StorageConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 storage configuration of a hub.
--
-- /See:/ 'newHubS3StorageConfig' smart constructor.
data HubS3StorageConfig = HubS3StorageConfig'
  { -- | The Amazon S3 bucket prefix for hosting hub content.
    s3OutputPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HubS3StorageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3OutputPath', 'hubS3StorageConfig_s3OutputPath' - The Amazon S3 bucket prefix for hosting hub content.
newHubS3StorageConfig ::
  HubS3StorageConfig
newHubS3StorageConfig =
  HubS3StorageConfig' {s3OutputPath = Prelude.Nothing}

-- | The Amazon S3 bucket prefix for hosting hub content.
hubS3StorageConfig_s3OutputPath :: Lens.Lens' HubS3StorageConfig (Prelude.Maybe Prelude.Text)
hubS3StorageConfig_s3OutputPath = Lens.lens (\HubS3StorageConfig' {s3OutputPath} -> s3OutputPath) (\s@HubS3StorageConfig' {} a -> s {s3OutputPath = a} :: HubS3StorageConfig)

instance Data.FromJSON HubS3StorageConfig where
  parseJSON =
    Data.withObject
      "HubS3StorageConfig"
      ( \x ->
          HubS3StorageConfig'
            Prelude.<$> (x Data..:? "S3OutputPath")
      )

instance Prelude.Hashable HubS3StorageConfig where
  hashWithSalt _salt HubS3StorageConfig' {..} =
    _salt `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData HubS3StorageConfig where
  rnf HubS3StorageConfig' {..} =
    Prelude.rnf s3OutputPath

instance Data.ToJSON HubS3StorageConfig where
  toJSON HubS3StorageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3OutputPath" Data..=) Prelude.<$> s3OutputPath]
      )
