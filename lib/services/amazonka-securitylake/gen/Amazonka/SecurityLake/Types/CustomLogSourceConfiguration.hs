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
-- Module      : Amazonka.SecurityLake.Types.CustomLogSourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.CustomLogSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AwsIdentity
import Amazonka.SecurityLake.Types.CustomLogSourceCrawlerConfiguration

-- | The configuration for the third-party custom source.
--
-- /See:/ 'newCustomLogSourceConfiguration' smart constructor.
data CustomLogSourceConfiguration = CustomLogSourceConfiguration'
  { -- | The configuration for the Glue Crawler for the third-party custom
    -- source.
    crawlerConfiguration :: CustomLogSourceCrawlerConfiguration,
    -- | The identity of the log provider for the third-party custom source.
    providerIdentity :: AwsIdentity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLogSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerConfiguration', 'customLogSourceConfiguration_crawlerConfiguration' - The configuration for the Glue Crawler for the third-party custom
-- source.
--
-- 'providerIdentity', 'customLogSourceConfiguration_providerIdentity' - The identity of the log provider for the third-party custom source.
newCustomLogSourceConfiguration ::
  -- | 'crawlerConfiguration'
  CustomLogSourceCrawlerConfiguration ->
  -- | 'providerIdentity'
  AwsIdentity ->
  CustomLogSourceConfiguration
newCustomLogSourceConfiguration
  pCrawlerConfiguration_
  pProviderIdentity_ =
    CustomLogSourceConfiguration'
      { crawlerConfiguration =
          pCrawlerConfiguration_,
        providerIdentity = pProviderIdentity_
      }

-- | The configuration for the Glue Crawler for the third-party custom
-- source.
customLogSourceConfiguration_crawlerConfiguration :: Lens.Lens' CustomLogSourceConfiguration CustomLogSourceCrawlerConfiguration
customLogSourceConfiguration_crawlerConfiguration = Lens.lens (\CustomLogSourceConfiguration' {crawlerConfiguration} -> crawlerConfiguration) (\s@CustomLogSourceConfiguration' {} a -> s {crawlerConfiguration = a} :: CustomLogSourceConfiguration)

-- | The identity of the log provider for the third-party custom source.
customLogSourceConfiguration_providerIdentity :: Lens.Lens' CustomLogSourceConfiguration AwsIdentity
customLogSourceConfiguration_providerIdentity = Lens.lens (\CustomLogSourceConfiguration' {providerIdentity} -> providerIdentity) (\s@CustomLogSourceConfiguration' {} a -> s {providerIdentity = a} :: CustomLogSourceConfiguration)

instance
  Prelude.Hashable
    CustomLogSourceConfiguration
  where
  hashWithSalt _salt CustomLogSourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` crawlerConfiguration
      `Prelude.hashWithSalt` providerIdentity

instance Prelude.NFData CustomLogSourceConfiguration where
  rnf CustomLogSourceConfiguration' {..} =
    Prelude.rnf crawlerConfiguration
      `Prelude.seq` Prelude.rnf providerIdentity

instance Data.ToJSON CustomLogSourceConfiguration where
  toJSON CustomLogSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "crawlerConfiguration"
                  Data..= crawlerConfiguration
              ),
            Prelude.Just
              ("providerIdentity" Data..= providerIdentity)
          ]
      )
