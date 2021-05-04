{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTData.Lens
  ( -- * Operations

    -- ** GetThingShadow
    getThingShadow_shadowName,
    getThingShadow_thingName,
    getThingShadowResponse_payload,
    getThingShadowResponse_httpStatus,

    -- ** Publish
    publish_payload,
    publish_qos,
    publish_topic,

    -- ** ListNamedShadowsForThing
    listNamedShadowsForThing_nextToken,
    listNamedShadowsForThing_pageSize,
    listNamedShadowsForThing_thingName,
    listNamedShadowsForThingResponse_nextToken,
    listNamedShadowsForThingResponse_timestamp,
    listNamedShadowsForThingResponse_results,
    listNamedShadowsForThingResponse_httpStatus,

    -- ** UpdateThingShadow
    updateThingShadow_shadowName,
    updateThingShadow_thingName,
    updateThingShadow_payload,
    updateThingShadowResponse_payload,
    updateThingShadowResponse_httpStatus,

    -- ** DeleteThingShadow
    deleteThingShadow_shadowName,
    deleteThingShadow_thingName,
    deleteThingShadowResponse_httpStatus,
    deleteThingShadowResponse_payload,

    -- * Types
  )
where

import Network.AWS.IoTData.DeleteThingShadow
import Network.AWS.IoTData.GetThingShadow
import Network.AWS.IoTData.ListNamedShadowsForThing
import Network.AWS.IoTData.Publish
import Network.AWS.IoTData.UpdateThingShadow
