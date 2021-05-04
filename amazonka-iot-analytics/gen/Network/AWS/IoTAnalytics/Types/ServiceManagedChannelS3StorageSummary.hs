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
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3StorageSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Used to store channel data in an S3 bucket managed by AWS IoT Analytics.
--
-- /See:/ 'newServiceManagedChannelS3StorageSummary' smart constructor.
data ServiceManagedChannelS3StorageSummary = ServiceManagedChannelS3StorageSummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceManagedChannelS3StorageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newServiceManagedChannelS3StorageSummary ::
  ServiceManagedChannelS3StorageSummary
newServiceManagedChannelS3StorageSummary =
  ServiceManagedChannelS3StorageSummary'

instance
  Prelude.FromJSON
    ServiceManagedChannelS3StorageSummary
  where
  parseJSON =
    Prelude.withObject
      "ServiceManagedChannelS3StorageSummary"
      ( \x ->
          Prelude.pure ServiceManagedChannelS3StorageSummary'
      )

instance
  Prelude.Hashable
    ServiceManagedChannelS3StorageSummary

instance
  Prelude.NFData
    ServiceManagedChannelS3StorageSummary
