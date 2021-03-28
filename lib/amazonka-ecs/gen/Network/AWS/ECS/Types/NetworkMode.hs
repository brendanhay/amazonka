{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.NetworkMode
  ( NetworkMode
    ( NetworkMode'
    , NetworkModeBridge
    , NetworkModeHost
    , NetworkModeAwsvpc
    , NetworkModeNone
    , fromNetworkMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype NetworkMode = NetworkMode'{fromNetworkMode :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern NetworkModeBridge :: NetworkMode
pattern NetworkModeBridge = NetworkMode' "bridge"

pattern NetworkModeHost :: NetworkMode
pattern NetworkModeHost = NetworkMode' "host"

pattern NetworkModeAwsvpc :: NetworkMode
pattern NetworkModeAwsvpc = NetworkMode' "awsvpc"

pattern NetworkModeNone :: NetworkMode
pattern NetworkModeNone = NetworkMode' "none"

{-# COMPLETE 
  NetworkModeBridge,

  NetworkModeHost,

  NetworkModeAwsvpc,

  NetworkModeNone,
  NetworkMode'
  #-}
