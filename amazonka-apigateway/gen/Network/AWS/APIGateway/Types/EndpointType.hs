{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | The endpoint type. The valid values are @EDGE@ for edge-optimized API
-- setup, most suitable for mobile applications; @REGIONAL@ for regional
-- API endpoint setup, most suitable for calling from AWS Region; and
-- @PRIVATE@ for private APIs.
newtype EndpointType = EndpointType'
  { fromEndpointType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
