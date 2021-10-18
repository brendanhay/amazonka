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
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetRequestConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ActivityStatus
import Network.AWS.EC2.Types.BatchState
import Network.AWS.EC2.Types.SpotFleetRequestConfigData
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Spot Fleet request.
--
-- /See:/ 'newSpotFleetRequestConfig' smart constructor.
data SpotFleetRequestConfig = SpotFleetRequestConfig'
  { -- | The state of the Spot Fleet request.
    spotFleetRequestState :: Prelude.Maybe BatchState,
    -- | The creation date and time of the request.
    createTime :: Prelude.Maybe Core.ISO8601,
    -- | The tags for a Spot Fleet resource.
    tags :: Prelude.Maybe [Tag],
    -- | The progress of the Spot Fleet request. If there is an error, the status
    -- is @error@. After all requests are placed, the status is
    -- @pending_fulfillment@. If the size of the fleet is equal to or greater
    -- than its target capacity, the status is @fulfilled@. If the size of the
    -- fleet is decreased, the status is @pending_termination@ while Spot
    -- Instances are terminating.
    activityStatus :: Prelude.Maybe ActivityStatus,
    -- | The configuration of the Spot Fleet request.
    spotFleetRequestConfig :: Prelude.Maybe SpotFleetRequestConfigData,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotFleetRequestConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotFleetRequestState', 'spotFleetRequestConfig_spotFleetRequestState' - The state of the Spot Fleet request.
--
-- 'createTime', 'spotFleetRequestConfig_createTime' - The creation date and time of the request.
--
-- 'tags', 'spotFleetRequestConfig_tags' - The tags for a Spot Fleet resource.
--
-- 'activityStatus', 'spotFleetRequestConfig_activityStatus' - The progress of the Spot Fleet request. If there is an error, the status
-- is @error@. After all requests are placed, the status is
-- @pending_fulfillment@. If the size of the fleet is equal to or greater
-- than its target capacity, the status is @fulfilled@. If the size of the
-- fleet is decreased, the status is @pending_termination@ while Spot
-- Instances are terminating.
--
-- 'spotFleetRequestConfig', 'spotFleetRequestConfig_spotFleetRequestConfig' - The configuration of the Spot Fleet request.
--
-- 'spotFleetRequestId', 'spotFleetRequestConfig_spotFleetRequestId' - The ID of the Spot Fleet request.
newSpotFleetRequestConfig ::
  SpotFleetRequestConfig
newSpotFleetRequestConfig =
  SpotFleetRequestConfig'
    { spotFleetRequestState =
        Prelude.Nothing,
      createTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      activityStatus = Prelude.Nothing,
      spotFleetRequestConfig = Prelude.Nothing,
      spotFleetRequestId = Prelude.Nothing
    }

-- | The state of the Spot Fleet request.
spotFleetRequestConfig_spotFleetRequestState :: Lens.Lens' SpotFleetRequestConfig (Prelude.Maybe BatchState)
spotFleetRequestConfig_spotFleetRequestState = Lens.lens (\SpotFleetRequestConfig' {spotFleetRequestState} -> spotFleetRequestState) (\s@SpotFleetRequestConfig' {} a -> s {spotFleetRequestState = a} :: SpotFleetRequestConfig)

-- | The creation date and time of the request.
spotFleetRequestConfig_createTime :: Lens.Lens' SpotFleetRequestConfig (Prelude.Maybe Prelude.UTCTime)
spotFleetRequestConfig_createTime = Lens.lens (\SpotFleetRequestConfig' {createTime} -> createTime) (\s@SpotFleetRequestConfig' {} a -> s {createTime = a} :: SpotFleetRequestConfig) Prelude.. Lens.mapping Core._Time

-- | The tags for a Spot Fleet resource.
spotFleetRequestConfig_tags :: Lens.Lens' SpotFleetRequestConfig (Prelude.Maybe [Tag])
spotFleetRequestConfig_tags = Lens.lens (\SpotFleetRequestConfig' {tags} -> tags) (\s@SpotFleetRequestConfig' {} a -> s {tags = a} :: SpotFleetRequestConfig) Prelude.. Lens.mapping Lens._Coerce

-- | The progress of the Spot Fleet request. If there is an error, the status
-- is @error@. After all requests are placed, the status is
-- @pending_fulfillment@. If the size of the fleet is equal to or greater
-- than its target capacity, the status is @fulfilled@. If the size of the
-- fleet is decreased, the status is @pending_termination@ while Spot
-- Instances are terminating.
spotFleetRequestConfig_activityStatus :: Lens.Lens' SpotFleetRequestConfig (Prelude.Maybe ActivityStatus)
spotFleetRequestConfig_activityStatus = Lens.lens (\SpotFleetRequestConfig' {activityStatus} -> activityStatus) (\s@SpotFleetRequestConfig' {} a -> s {activityStatus = a} :: SpotFleetRequestConfig)

-- | The configuration of the Spot Fleet request.
spotFleetRequestConfig_spotFleetRequestConfig :: Lens.Lens' SpotFleetRequestConfig (Prelude.Maybe SpotFleetRequestConfigData)
spotFleetRequestConfig_spotFleetRequestConfig = Lens.lens (\SpotFleetRequestConfig' {spotFleetRequestConfig} -> spotFleetRequestConfig) (\s@SpotFleetRequestConfig' {} a -> s {spotFleetRequestConfig = a} :: SpotFleetRequestConfig)

-- | The ID of the Spot Fleet request.
spotFleetRequestConfig_spotFleetRequestId :: Lens.Lens' SpotFleetRequestConfig (Prelude.Maybe Prelude.Text)
spotFleetRequestConfig_spotFleetRequestId = Lens.lens (\SpotFleetRequestConfig' {spotFleetRequestId} -> spotFleetRequestId) (\s@SpotFleetRequestConfig' {} a -> s {spotFleetRequestId = a} :: SpotFleetRequestConfig)

instance Core.FromXML SpotFleetRequestConfig where
  parseXML x =
    SpotFleetRequestConfig'
      Prelude.<$> (x Core..@? "spotFleetRequestState")
      Prelude.<*> (x Core..@? "createTime")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "activityStatus")
      Prelude.<*> (x Core..@? "spotFleetRequestConfig")
      Prelude.<*> (x Core..@? "spotFleetRequestId")

instance Prelude.Hashable SpotFleetRequestConfig

instance Prelude.NFData SpotFleetRequestConfig
