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
-- Module      : Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ServiceManagedChannelS3Storage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Use this to store channel data in an S3 bucket managed by AWS IoT
-- Analytics. You cannot change the choice of service-managed or
-- customer-managed S3 storage after the channel is created.
--
-- /See:/ 'newServiceManagedChannelS3Storage' smart constructor.
data ServiceManagedChannelS3Storage = ServiceManagedChannelS3Storage'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceManagedChannelS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newServiceManagedChannelS3Storage ::
  ServiceManagedChannelS3Storage
newServiceManagedChannelS3Storage =
  ServiceManagedChannelS3Storage'

instance
  Prelude.FromJSON
    ServiceManagedChannelS3Storage
  where
  parseJSON =
    Prelude.withObject
      "ServiceManagedChannelS3Storage"
      (\x -> Prelude.pure ServiceManagedChannelS3Storage')

instance
  Prelude.Hashable
    ServiceManagedChannelS3Storage

instance
  Prelude.NFData
    ServiceManagedChannelS3Storage

instance
  Prelude.ToJSON
    ServiceManagedChannelS3Storage
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)
