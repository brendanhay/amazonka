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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype RouteAnalysisStatus = RouteAnalysisStatus'
  { fromRouteAnalysisStatus ::
      Core.Text
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
