{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTSiteWise.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.DescribeAsset
import Network.AWS.IoTSiteWise.DescribeAssetModel
import Network.AWS.IoTSiteWise.DescribePortal
import Network.AWS.IoTSiteWise.Lens
import Network.AWS.IoTSiteWise.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.IoTSiteWise.DescribeAssetModel' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetModelNotExists :: Core.Wait DescribeAssetModel
newAssetModelNotExists =
  Core.Wait
    { Core._waitName = "AssetModelNotExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.IoTSiteWise.DescribeAsset' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetNotExists :: Core.Wait DescribeAsset
newAssetNotExists =
  Core.Wait
    { Core._waitName = "AssetNotExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.IoTSiteWise.DescribeAssetModel' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetModelActive :: Core.Wait DescribeAssetModel
newAssetModelActive =
  Core.Wait
    { Core._waitName = "AssetModelActive",
      Core._waitAttempts = 20,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeAssetModelResponse_assetModelStatus
                Prelude.. assetModelStatus_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeAssetModelResponse_assetModelStatus
                Prelude.. assetModelStatus_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.IoTSiteWise.DescribePortal' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newPortalNotExists :: Core.Wait DescribePortal
newPortalNotExists =
  Core.Wait
    { Core._waitName = "PortalNotExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.IoTSiteWise.DescribeAsset' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetActive :: Core.Wait DescribeAsset
newAssetActive =
  Core.Wait
    { Core._waitName = "AssetActive",
      Core._waitAttempts = 20,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeAssetResponse_assetStatus
                Prelude.. assetStatus_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeAssetResponse_assetStatus
                Prelude.. assetStatus_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.IoTSiteWise.DescribePortal' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newPortalActive :: Core.Wait DescribePortal
newPortalActive =
  Core.Wait
    { Core._waitName = "PortalActive",
      Core._waitAttempts = 20,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describePortalResponse_portalStatus
                Prelude.. portalStatus_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
