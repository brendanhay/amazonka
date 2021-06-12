{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.EndpointType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.EndpointType
  ( EndpointType
      ( ..,
        EndpointType_EDGE,
        EndpointType_PRIVATE,
        EndpointType_REGIONAL
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The endpoint type. The valid values are @EDGE@ for edge-optimized API
-- setup, most suitable for mobile applications; @REGIONAL@ for regional
-- API endpoint setup, most suitable for calling from AWS Region; and
-- @PRIVATE@ for private APIs.
newtype EndpointType = EndpointType'
  { fromEndpointType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern EndpointType_EDGE :: EndpointType
pattern EndpointType_EDGE = EndpointType' "EDGE"

pattern EndpointType_PRIVATE :: EndpointType
pattern EndpointType_PRIVATE = EndpointType' "PRIVATE"

pattern EndpointType_REGIONAL :: EndpointType
pattern EndpointType_REGIONAL = EndpointType' "REGIONAL"

{-# COMPLETE
  EndpointType_EDGE,
  EndpointType_PRIVATE,
  EndpointType_REGIONAL,
  EndpointType'
  #-}
