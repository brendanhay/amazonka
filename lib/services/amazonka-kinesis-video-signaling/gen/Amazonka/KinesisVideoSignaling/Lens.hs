{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoSignaling.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideoSignaling.Lens
  ( -- * Operations

    -- ** GetIceServerConfig
    getIceServerConfig_clientId,
    getIceServerConfig_service,
    getIceServerConfig_username,
    getIceServerConfig_channelARN,
    getIceServerConfigResponse_iceServerList,
    getIceServerConfigResponse_httpStatus,

    -- ** SendAlexaOfferToMaster
    sendAlexaOfferToMaster_channelARN,
    sendAlexaOfferToMaster_senderClientId,
    sendAlexaOfferToMaster_messagePayload,
    sendAlexaOfferToMasterResponse_answer,
    sendAlexaOfferToMasterResponse_httpStatus,

    -- * Types

    -- ** IceServer
    iceServer_password,
    iceServer_ttl,
    iceServer_uris,
    iceServer_username,
  )
where

import Amazonka.KinesisVideoSignaling.GetIceServerConfig
import Amazonka.KinesisVideoSignaling.SendAlexaOfferToMaster
import Amazonka.KinesisVideoSignaling.Types.IceServer
