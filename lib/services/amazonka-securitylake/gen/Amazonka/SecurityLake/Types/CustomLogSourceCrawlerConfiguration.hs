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
-- Module      : Amazonka.SecurityLake.Types.CustomLogSourceCrawlerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.CustomLogSourceCrawlerConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the Glue Crawler for the third-party custom
-- source.
--
-- /See:/ 'newCustomLogSourceCrawlerConfiguration' smart constructor.
data CustomLogSourceCrawlerConfiguration = CustomLogSourceCrawlerConfiguration'
  { -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role to be used by the Glue crawler. The recommended IAM policies
    -- are:
    --
    -- -   The managed policy @AWSGlueServiceRole@
    --
    -- -   A custom policy granting access to your Amazon S3 Data Lake
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLogSourceCrawlerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'customLogSourceCrawlerConfiguration_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be used by the Glue crawler. The recommended IAM policies
-- are:
--
-- -   The managed policy @AWSGlueServiceRole@
--
-- -   A custom policy granting access to your Amazon S3 Data Lake
newCustomLogSourceCrawlerConfiguration ::
  -- | 'roleArn'
  Prelude.Text ->
  CustomLogSourceCrawlerConfiguration
newCustomLogSourceCrawlerConfiguration pRoleArn_ =
  CustomLogSourceCrawlerConfiguration'
    { roleArn =
        pRoleArn_
    }

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role to be used by the Glue crawler. The recommended IAM policies
-- are:
--
-- -   The managed policy @AWSGlueServiceRole@
--
-- -   A custom policy granting access to your Amazon S3 Data Lake
customLogSourceCrawlerConfiguration_roleArn :: Lens.Lens' CustomLogSourceCrawlerConfiguration Prelude.Text
customLogSourceCrawlerConfiguration_roleArn = Lens.lens (\CustomLogSourceCrawlerConfiguration' {roleArn} -> roleArn) (\s@CustomLogSourceCrawlerConfiguration' {} a -> s {roleArn = a} :: CustomLogSourceCrawlerConfiguration)

instance
  Prelude.Hashable
    CustomLogSourceCrawlerConfiguration
  where
  hashWithSalt
    _salt
    CustomLogSourceCrawlerConfiguration' {..} =
      _salt `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CustomLogSourceCrawlerConfiguration
  where
  rnf CustomLogSourceCrawlerConfiguration' {..} =
    Prelude.rnf roleArn

instance
  Data.ToJSON
    CustomLogSourceCrawlerConfiguration
  where
  toJSON CustomLogSourceCrawlerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("roleArn" Data..= roleArn)]
      )
