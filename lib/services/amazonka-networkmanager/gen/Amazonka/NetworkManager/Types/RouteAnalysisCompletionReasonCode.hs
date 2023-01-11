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
-- Module      : Amazonka.NetworkManager.Types.RouteAnalysisCompletionReasonCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteAnalysisCompletionReasonCode
  ( RouteAnalysisCompletionReasonCode
      ( ..,
        RouteAnalysisCompletionReasonCode_BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND,
        RouteAnalysisCompletionReasonCode_CYCLIC_PATH_DETECTED,
        RouteAnalysisCompletionReasonCode_INACTIVE_ROUTE_FOR_DESTINATION_FOUND,
        RouteAnalysisCompletionReasonCode_MAX_HOPS_EXCEEDED,
        RouteAnalysisCompletionReasonCode_NO_DESTINATION_ARN_PROVIDED,
        RouteAnalysisCompletionReasonCode_POSSIBLE_MIDDLEBOX,
        RouteAnalysisCompletionReasonCode_ROUTE_NOT_FOUND,
        RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_ATTACH_ARN_NO_MATCH,
        RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_FOUND,
        RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_IN_TRANSIT_GATEWAY,
        RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_STABLE_ROUTE_TABLE_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RouteAnalysisCompletionReasonCode = RouteAnalysisCompletionReasonCode'
  { fromRouteAnalysisCompletionReasonCode ::
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

pattern RouteAnalysisCompletionReasonCode_BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND = RouteAnalysisCompletionReasonCode' "BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND"

pattern RouteAnalysisCompletionReasonCode_CYCLIC_PATH_DETECTED :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_CYCLIC_PATH_DETECTED = RouteAnalysisCompletionReasonCode' "CYCLIC_PATH_DETECTED"

pattern RouteAnalysisCompletionReasonCode_INACTIVE_ROUTE_FOR_DESTINATION_FOUND :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_INACTIVE_ROUTE_FOR_DESTINATION_FOUND = RouteAnalysisCompletionReasonCode' "INACTIVE_ROUTE_FOR_DESTINATION_FOUND"

pattern RouteAnalysisCompletionReasonCode_MAX_HOPS_EXCEEDED :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_MAX_HOPS_EXCEEDED = RouteAnalysisCompletionReasonCode' "MAX_HOPS_EXCEEDED"

pattern RouteAnalysisCompletionReasonCode_NO_DESTINATION_ARN_PROVIDED :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_NO_DESTINATION_ARN_PROVIDED = RouteAnalysisCompletionReasonCode' "NO_DESTINATION_ARN_PROVIDED"

pattern RouteAnalysisCompletionReasonCode_POSSIBLE_MIDDLEBOX :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_POSSIBLE_MIDDLEBOX = RouteAnalysisCompletionReasonCode' "POSSIBLE_MIDDLEBOX"

pattern RouteAnalysisCompletionReasonCode_ROUTE_NOT_FOUND :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_ROUTE_NOT_FOUND = RouteAnalysisCompletionReasonCode' "ROUTE_NOT_FOUND"

pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_ATTACH_ARN_NO_MATCH :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_ATTACH_ARN_NO_MATCH = RouteAnalysisCompletionReasonCode' "TRANSIT_GATEWAY_ATTACHMENT_ATTACH_ARN_NO_MATCH"

pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_FOUND :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_FOUND = RouteAnalysisCompletionReasonCode' "TRANSIT_GATEWAY_ATTACHMENT_NOT_FOUND"

pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_IN_TRANSIT_GATEWAY :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_IN_TRANSIT_GATEWAY = RouteAnalysisCompletionReasonCode' "TRANSIT_GATEWAY_ATTACHMENT_NOT_IN_TRANSIT_GATEWAY"

pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_STABLE_ROUTE_TABLE_NOT_FOUND :: RouteAnalysisCompletionReasonCode
pattern RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_STABLE_ROUTE_TABLE_NOT_FOUND = RouteAnalysisCompletionReasonCode' "TRANSIT_GATEWAY_ATTACHMENT_STABLE_ROUTE_TABLE_NOT_FOUND"

{-# COMPLETE
  RouteAnalysisCompletionReasonCode_BLACKHOLE_ROUTE_FOR_DESTINATION_FOUND,
  RouteAnalysisCompletionReasonCode_CYCLIC_PATH_DETECTED,
  RouteAnalysisCompletionReasonCode_INACTIVE_ROUTE_FOR_DESTINATION_FOUND,
  RouteAnalysisCompletionReasonCode_MAX_HOPS_EXCEEDED,
  RouteAnalysisCompletionReasonCode_NO_DESTINATION_ARN_PROVIDED,
  RouteAnalysisCompletionReasonCode_POSSIBLE_MIDDLEBOX,
  RouteAnalysisCompletionReasonCode_ROUTE_NOT_FOUND,
  RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_ATTACH_ARN_NO_MATCH,
  RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_FOUND,
  RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_NOT_IN_TRANSIT_GATEWAY,
  RouteAnalysisCompletionReasonCode_TRANSIT_GATEWAY_ATTACHMENT_STABLE_ROUTE_TABLE_NOT_FOUND,
  RouteAnalysisCompletionReasonCode'
  #-}
