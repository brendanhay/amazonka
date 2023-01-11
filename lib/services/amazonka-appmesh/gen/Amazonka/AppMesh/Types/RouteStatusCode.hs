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
-- Module      : Amazonka.AppMesh.Types.RouteStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.RouteStatusCode
  ( RouteStatusCode
      ( ..,
        RouteStatusCode_ACTIVE,
        RouteStatusCode_DELETED,
        RouteStatusCode_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RouteStatusCode = RouteStatusCode'
  { fromRouteStatusCode ::
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

pattern RouteStatusCode_ACTIVE :: RouteStatusCode
pattern RouteStatusCode_ACTIVE = RouteStatusCode' "ACTIVE"

pattern RouteStatusCode_DELETED :: RouteStatusCode
pattern RouteStatusCode_DELETED = RouteStatusCode' "DELETED"

pattern RouteStatusCode_INACTIVE :: RouteStatusCode
pattern RouteStatusCode_INACTIVE = RouteStatusCode' "INACTIVE"

{-# COMPLETE
  RouteStatusCode_ACTIVE,
  RouteStatusCode_DELETED,
  RouteStatusCode_INACTIVE,
  RouteStatusCode'
  #-}
