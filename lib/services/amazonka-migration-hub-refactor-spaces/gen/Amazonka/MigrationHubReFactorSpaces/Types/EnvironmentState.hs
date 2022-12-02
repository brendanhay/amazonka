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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.EnvironmentState
  ( EnvironmentState
      ( ..,
        EnvironmentState_ACTIVE,
        EnvironmentState_CREATING,
        EnvironmentState_DELETING,
        EnvironmentState_FAILED
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

pattern EnvironmentState_ACTIVE :: EnvironmentState
pattern EnvironmentState_ACTIVE = EnvironmentState' "ACTIVE"

pattern EnvironmentState_CREATING :: EnvironmentState
pattern EnvironmentState_CREATING = EnvironmentState' "CREATING"

pattern EnvironmentState_DELETING :: EnvironmentState
pattern EnvironmentState_DELETING = EnvironmentState' "DELETING"

pattern EnvironmentState_FAILED :: EnvironmentState
pattern EnvironmentState_FAILED = EnvironmentState' "FAILED"

{-# COMPLETE
  EnvironmentState_ACTIVE,
  EnvironmentState_CREATING,
  EnvironmentState_DELETING,
  EnvironmentState_FAILED,
  EnvironmentState'
  #-}
