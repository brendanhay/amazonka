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
-- Module      : Amazonka.AppConfig.Types.EnvironmentState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.EnvironmentState
  ( EnvironmentState
      ( ..,
        EnvironmentState_DEPLOYING,
        EnvironmentState_READY_FOR_DEPLOYMENT,
        EnvironmentState_ROLLED_BACK,
        EnvironmentState_ROLLING_BACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentState = EnvironmentState'
  { fromEnvironmentState ::
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

pattern EnvironmentState_DEPLOYING :: EnvironmentState
pattern EnvironmentState_DEPLOYING = EnvironmentState' "DEPLOYING"

pattern EnvironmentState_READY_FOR_DEPLOYMENT :: EnvironmentState
pattern EnvironmentState_READY_FOR_DEPLOYMENT = EnvironmentState' "READY_FOR_DEPLOYMENT"

pattern EnvironmentState_ROLLED_BACK :: EnvironmentState
pattern EnvironmentState_ROLLED_BACK = EnvironmentState' "ROLLED_BACK"

pattern EnvironmentState_ROLLING_BACK :: EnvironmentState
pattern EnvironmentState_ROLLING_BACK = EnvironmentState' "ROLLING_BACK"

{-# COMPLETE
  EnvironmentState_DEPLOYING,
  EnvironmentState_READY_FOR_DEPLOYMENT,
  EnvironmentState_ROLLED_BACK,
  EnvironmentState_ROLLING_BACK,
  EnvironmentState'
  #-}
