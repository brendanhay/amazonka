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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayEndpointType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayEndpointType
  ( ApiGatewayEndpointType
      ( ..,
        ApiGatewayEndpointType_PRIVATE,
        ApiGatewayEndpointType_REGIONAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApiGatewayEndpointType = ApiGatewayEndpointType'
  { fromApiGatewayEndpointType ::
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

pattern ApiGatewayEndpointType_PRIVATE :: ApiGatewayEndpointType
pattern ApiGatewayEndpointType_PRIVATE = ApiGatewayEndpointType' "PRIVATE"

pattern ApiGatewayEndpointType_REGIONAL :: ApiGatewayEndpointType
pattern ApiGatewayEndpointType_REGIONAL = ApiGatewayEndpointType' "REGIONAL"

{-# COMPLETE
  ApiGatewayEndpointType_PRIVATE,
  ApiGatewayEndpointType_REGIONAL,
  ApiGatewayEndpointType'
  #-}
