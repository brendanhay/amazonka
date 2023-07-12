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
-- Module      : Amazonka.Location.Types.RouteMatrixErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.RouteMatrixErrorCode
  ( RouteMatrixErrorCode
      ( ..,
        RouteMatrixErrorCode_DeparturePositionNotFound,
        RouteMatrixErrorCode_DestinationPositionNotFound,
        RouteMatrixErrorCode_OtherValidationError,
        RouteMatrixErrorCode_PositionsNotFound,
        RouteMatrixErrorCode_RouteNotFound,
        RouteMatrixErrorCode_RouteTooLong
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RouteMatrixErrorCode = RouteMatrixErrorCode'
  { fromRouteMatrixErrorCode ::
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

pattern RouteMatrixErrorCode_DeparturePositionNotFound :: RouteMatrixErrorCode
pattern RouteMatrixErrorCode_DeparturePositionNotFound = RouteMatrixErrorCode' "DeparturePositionNotFound"

pattern RouteMatrixErrorCode_DestinationPositionNotFound :: RouteMatrixErrorCode
pattern RouteMatrixErrorCode_DestinationPositionNotFound = RouteMatrixErrorCode' "DestinationPositionNotFound"

pattern RouteMatrixErrorCode_OtherValidationError :: RouteMatrixErrorCode
pattern RouteMatrixErrorCode_OtherValidationError = RouteMatrixErrorCode' "OtherValidationError"

pattern RouteMatrixErrorCode_PositionsNotFound :: RouteMatrixErrorCode
pattern RouteMatrixErrorCode_PositionsNotFound = RouteMatrixErrorCode' "PositionsNotFound"

pattern RouteMatrixErrorCode_RouteNotFound :: RouteMatrixErrorCode
pattern RouteMatrixErrorCode_RouteNotFound = RouteMatrixErrorCode' "RouteNotFound"

pattern RouteMatrixErrorCode_RouteTooLong :: RouteMatrixErrorCode
pattern RouteMatrixErrorCode_RouteTooLong = RouteMatrixErrorCode' "RouteTooLong"

{-# COMPLETE
  RouteMatrixErrorCode_DeparturePositionNotFound,
  RouteMatrixErrorCode_DestinationPositionNotFound,
  RouteMatrixErrorCode_OtherValidationError,
  RouteMatrixErrorCode_PositionsNotFound,
  RouteMatrixErrorCode_RouteNotFound,
  RouteMatrixErrorCode_RouteTooLong,
  RouteMatrixErrorCode'
  #-}
