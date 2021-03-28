{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.EndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.EndpointType
  ( EndpointType
    ( EndpointType'
    , EndpointTypeRegional
    , EndpointTypeEdge
    , EndpointTypePrivate
    , fromEndpointType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The endpoint type. The valid values are @EDGE@ for edge-optimized API setup, most suitable for mobile applications; @REGIONAL@ for regional API endpoint setup, most suitable for calling from AWS Region; and @PRIVATE@ for private APIs.
newtype EndpointType = EndpointType'{fromEndpointType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern EndpointTypeRegional :: EndpointType
pattern EndpointTypeRegional = EndpointType' "REGIONAL"

pattern EndpointTypeEdge :: EndpointType
pattern EndpointTypeEdge = EndpointType' "EDGE"

pattern EndpointTypePrivate :: EndpointType
pattern EndpointTypePrivate = EndpointType' "PRIVATE"

{-# COMPLETE 
  EndpointTypeRegional,

  EndpointTypeEdge,

  EndpointTypePrivate,
  EndpointType'
  #-}
