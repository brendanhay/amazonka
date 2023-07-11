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
-- Module      : Amazonka.APIGateway.Types.EndpointType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.EndpointType
  ( EndpointType
      ( ..,
        EndpointType_EDGE,
        EndpointType_PRIVATE,
        EndpointType_REGIONAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The endpoint type. The valid values are @EDGE@ for edge-optimized API
-- setup, most suitable for mobile applications; @REGIONAL@ for regional
-- API endpoint setup, most suitable for calling from AWS Region; and
-- @PRIVATE@ for private APIs.
newtype EndpointType = EndpointType'
  { fromEndpointType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
