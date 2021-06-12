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
-- Module      : Network.AWS.EC2.Types.ConnectionLogOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionLogOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the client connection logging options for the Client VPN
-- endpoint.
--
-- /See:/ 'newConnectionLogOptions' smart constructor.
data ConnectionLogOptions = ConnectionLogOptions'
  { -- | The name of the CloudWatch Logs log stream to which the connection data
    -- is published.
    cloudwatchLogStream :: Core.Maybe Core.Text,
    -- | Indicates whether connection logging is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The name of the CloudWatch Logs log group. Required if connection
    -- logging is enabled.
    cloudwatchLogGroup :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectionLogOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudwatchLogStream', 'connectionLogOptions_cloudwatchLogStream' - The name of the CloudWatch Logs log stream to which the connection data
-- is published.
--
-- 'enabled', 'connectionLogOptions_enabled' - Indicates whether connection logging is enabled.
--
-- 'cloudwatchLogGroup', 'connectionLogOptions_cloudwatchLogGroup' - The name of the CloudWatch Logs log group. Required if connection
-- logging is enabled.
newConnectionLogOptions ::
  ConnectionLogOptions
newConnectionLogOptions =
  ConnectionLogOptions'
    { cloudwatchLogStream =
        Core.Nothing,
      enabled = Core.Nothing,
      cloudwatchLogGroup = Core.Nothing
    }

-- | The name of the CloudWatch Logs log stream to which the connection data
-- is published.
connectionLogOptions_cloudwatchLogStream :: Lens.Lens' ConnectionLogOptions (Core.Maybe Core.Text)
connectionLogOptions_cloudwatchLogStream = Lens.lens (\ConnectionLogOptions' {cloudwatchLogStream} -> cloudwatchLogStream) (\s@ConnectionLogOptions' {} a -> s {cloudwatchLogStream = a} :: ConnectionLogOptions)

-- | Indicates whether connection logging is enabled.
connectionLogOptions_enabled :: Lens.Lens' ConnectionLogOptions (Core.Maybe Core.Bool)
connectionLogOptions_enabled = Lens.lens (\ConnectionLogOptions' {enabled} -> enabled) (\s@ConnectionLogOptions' {} a -> s {enabled = a} :: ConnectionLogOptions)

-- | The name of the CloudWatch Logs log group. Required if connection
-- logging is enabled.
connectionLogOptions_cloudwatchLogGroup :: Lens.Lens' ConnectionLogOptions (Core.Maybe Core.Text)
connectionLogOptions_cloudwatchLogGroup = Lens.lens (\ConnectionLogOptions' {cloudwatchLogGroup} -> cloudwatchLogGroup) (\s@ConnectionLogOptions' {} a -> s {cloudwatchLogGroup = a} :: ConnectionLogOptions)

instance Core.Hashable ConnectionLogOptions

instance Core.NFData ConnectionLogOptions

instance Core.ToQuery ConnectionLogOptions where
  toQuery ConnectionLogOptions' {..} =
    Core.mconcat
      [ "CloudwatchLogStream" Core.=: cloudwatchLogStream,
        "Enabled" Core.=: enabled,
        "CloudwatchLogGroup" Core.=: cloudwatchLogGroup
      ]
