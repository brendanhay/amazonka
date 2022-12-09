{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSiteWise.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.DescribeAsset
import Amazonka.IoTSiteWise.DescribeAssetModel
import Amazonka.IoTSiteWise.DescribePortal
import Amazonka.IoTSiteWise.Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.IoTSiteWise.DescribeAsset' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetActive :: Core.Wait DescribeAsset
newAssetActive =
  Core.Wait
    { Core.name = "AssetActive",
      Core.attempts = 20,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeAssetResponse_assetStatus
                Prelude.. assetStatus_state
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeAssetResponse_assetStatus
                Prelude.. assetStatus_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.IoTSiteWise.DescribeAssetModel' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetModelActive :: Core.Wait DescribeAssetModel
newAssetModelActive =
  Core.Wait
    { Core.name = "AssetModelActive",
      Core.attempts = 20,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeAssetModelResponse_assetModelStatus
                Prelude.. assetModelStatus_state
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeAssetModelResponse_assetModelStatus
                Prelude.. assetModelStatus_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.IoTSiteWise.DescribeAssetModel' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetModelNotExists :: Core.Wait DescribeAssetModel
newAssetModelNotExists =
  Core.Wait
    { Core.name = "AssetModelNotExists",
      Core.attempts = 20,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.IoTSiteWise.DescribeAsset' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newAssetNotExists :: Core.Wait DescribeAsset
newAssetNotExists =
  Core.Wait
    { Core.name = "AssetNotExists",
      Core.attempts = 20,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.IoTSiteWise.DescribePortal' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newPortalActive :: Core.Wait DescribePortal
newPortalActive =
  Core.Wait
    { Core.name = "PortalActive",
      Core.attempts = 20,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describePortalResponse_portalStatus
                Prelude.. portalStatus_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.IoTSiteWise.DescribePortal' every 3 seconds until a successful state is reached. An error is returned after 20 failed checks.
newPortalNotExists :: Core.Wait DescribePortal
newPortalNotExists =
  Core.Wait
    { Core.name = "PortalNotExists",
      Core.attempts = 20,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }
