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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteStatusCode
  ( GatewayRouteStatusCode
      ( ..,
        GatewayRouteStatusCode_ACTIVE,
        GatewayRouteStatusCode_DELETED,
        GatewayRouteStatusCode_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GatewayRouteStatusCode = GatewayRouteStatusCode'
  { fromGatewayRouteStatusCode ::
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

pattern GatewayRouteStatusCode_ACTIVE :: GatewayRouteStatusCode
pattern GatewayRouteStatusCode_ACTIVE = GatewayRouteStatusCode' "ACTIVE"

pattern GatewayRouteStatusCode_DELETED :: GatewayRouteStatusCode
pattern GatewayRouteStatusCode_DELETED = GatewayRouteStatusCode' "DELETED"

pattern GatewayRouteStatusCode_INACTIVE :: GatewayRouteStatusCode
pattern GatewayRouteStatusCode_INACTIVE = GatewayRouteStatusCode' "INACTIVE"

{-# COMPLETE
  GatewayRouteStatusCode_ACTIVE,
  GatewayRouteStatusCode_DELETED,
  GatewayRouteStatusCode_INACTIVE,
  GatewayRouteStatusCode'
  #-}
