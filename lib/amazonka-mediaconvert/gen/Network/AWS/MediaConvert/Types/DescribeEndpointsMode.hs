{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DescribeEndpointsMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DescribeEndpointsMode
  ( DescribeEndpointsMode
    ( DescribeEndpointsMode'
    , DescribeEndpointsModeDefault
    , DescribeEndpointsModeGetOnly
    , fromDescribeEndpointsMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
newtype DescribeEndpointsMode = DescribeEndpointsMode'{fromDescribeEndpointsMode
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern DescribeEndpointsModeDefault :: DescribeEndpointsMode
pattern DescribeEndpointsModeDefault = DescribeEndpointsMode' "DEFAULT"

pattern DescribeEndpointsModeGetOnly :: DescribeEndpointsMode
pattern DescribeEndpointsModeGetOnly = DescribeEndpointsMode' "GET_ONLY"

{-# COMPLETE 
  DescribeEndpointsModeDefault,

  DescribeEndpointsModeGetOnly,
  DescribeEndpointsMode'
  #-}
