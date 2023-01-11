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
-- Module      : Amazonka.NetworkManager.Types.RouteAnalysisStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteAnalysisStatus
  ( RouteAnalysisStatus
      ( ..,
        RouteAnalysisStatus_COMPLETED,
        RouteAnalysisStatus_FAILED,
        RouteAnalysisStatus_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RouteAnalysisStatus = RouteAnalysisStatus'
  { fromRouteAnalysisStatus ::
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

pattern RouteAnalysisStatus_COMPLETED :: RouteAnalysisStatus
pattern RouteAnalysisStatus_COMPLETED = RouteAnalysisStatus' "COMPLETED"

pattern RouteAnalysisStatus_FAILED :: RouteAnalysisStatus
pattern RouteAnalysisStatus_FAILED = RouteAnalysisStatus' "FAILED"

pattern RouteAnalysisStatus_RUNNING :: RouteAnalysisStatus
pattern RouteAnalysisStatus_RUNNING = RouteAnalysisStatus' "RUNNING"

{-# COMPLETE
  RouteAnalysisStatus_COMPLETED,
  RouteAnalysisStatus_FAILED,
  RouteAnalysisStatus_RUNNING,
  RouteAnalysisStatus'
  #-}
