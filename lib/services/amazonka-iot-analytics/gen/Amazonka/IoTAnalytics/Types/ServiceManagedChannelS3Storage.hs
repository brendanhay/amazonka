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
-- Module      : Amazonka.IoTAnalytics.Types.ServiceManagedChannelS3Storage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ServiceManagedChannelS3Storage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Used to store channel data in an S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of S3 storage after the data store is created.
--
-- /See:/ 'newServiceManagedChannelS3Storage' smart constructor.
data ServiceManagedChannelS3Storage = ServiceManagedChannelS3Storage'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceManagedChannelS3Storage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newServiceManagedChannelS3Storage ::
  ServiceManagedChannelS3Storage
newServiceManagedChannelS3Storage =
  ServiceManagedChannelS3Storage'

instance Data.FromJSON ServiceManagedChannelS3Storage where
  parseJSON =
    Data.withObject
      "ServiceManagedChannelS3Storage"
      (\x -> Prelude.pure ServiceManagedChannelS3Storage')

instance
  Prelude.Hashable
    ServiceManagedChannelS3Storage
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    ServiceManagedChannelS3Storage
  where
  rnf _ = ()

instance Data.ToJSON ServiceManagedChannelS3Storage where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
