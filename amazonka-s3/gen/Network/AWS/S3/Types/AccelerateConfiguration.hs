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
-- Module      : Network.AWS.S3.Types.AccelerateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AccelerateConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.BucketAccelerateStatus

-- | Configures the transfer acceleration state for an Amazon S3 bucket. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Amazon S3 Transfer Acceleration>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- /See:/ 'newAccelerateConfiguration' smart constructor.
data AccelerateConfiguration = AccelerateConfiguration'
  { -- | Specifies the transfer acceleration status of the bucket.
    status :: Core.Maybe BucketAccelerateStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccelerateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'accelerateConfiguration_status' - Specifies the transfer acceleration status of the bucket.
newAccelerateConfiguration ::
  AccelerateConfiguration
newAccelerateConfiguration =
  AccelerateConfiguration' {status = Core.Nothing}

-- | Specifies the transfer acceleration status of the bucket.
accelerateConfiguration_status :: Lens.Lens' AccelerateConfiguration (Core.Maybe BucketAccelerateStatus)
accelerateConfiguration_status = Lens.lens (\AccelerateConfiguration' {status} -> status) (\s@AccelerateConfiguration' {} a -> s {status = a} :: AccelerateConfiguration)

instance Core.Hashable AccelerateConfiguration

instance Core.NFData AccelerateConfiguration

instance Core.ToXML AccelerateConfiguration where
  toXML AccelerateConfiguration' {..} =
    Core.mconcat ["Status" Core.@= status]
