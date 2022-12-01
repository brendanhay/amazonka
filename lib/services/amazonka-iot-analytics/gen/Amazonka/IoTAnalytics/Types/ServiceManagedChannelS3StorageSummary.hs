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
-- Module      : Amazonka.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Used to store channel data in an S3 bucket managed by IoT Analytics.
--
-- /See:/ 'newServiceManagedChannelS3StorageSummary' smart constructor.
data ServiceManagedChannelS3StorageSummary = ServiceManagedChannelS3StorageSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceManagedChannelS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newServiceManagedChannelS3StorageSummary ::
  ServiceManagedChannelS3StorageSummary
newServiceManagedChannelS3StorageSummary =
  ServiceManagedChannelS3StorageSummary'

instance
  Core.FromJSON
    ServiceManagedChannelS3StorageSummary
  where
  parseJSON =
    Core.withObject
      "ServiceManagedChannelS3StorageSummary"
      ( \x ->
          Prelude.pure ServiceManagedChannelS3StorageSummary'
      )

instance
  Prelude.Hashable
    ServiceManagedChannelS3StorageSummary
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    ServiceManagedChannelS3StorageSummary
  where
  rnf _ = ()
