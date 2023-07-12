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
-- Module      : Amazonka.Nimble.Types.LaunchProfileState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileState
  ( LaunchProfileState
      ( ..,
        LaunchProfileState_CREATE_FAILED,
        LaunchProfileState_CREATE_IN_PROGRESS,
        LaunchProfileState_DELETED,
        LaunchProfileState_DELETE_FAILED,
        LaunchProfileState_DELETE_IN_PROGRESS,
        LaunchProfileState_READY,
        LaunchProfileState_UPDATE_FAILED,
        LaunchProfileState_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchProfileState = LaunchProfileState'
  { fromLaunchProfileState ::
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

pattern LaunchProfileState_CREATE_FAILED :: LaunchProfileState
pattern LaunchProfileState_CREATE_FAILED = LaunchProfileState' "CREATE_FAILED"

pattern LaunchProfileState_CREATE_IN_PROGRESS :: LaunchProfileState
pattern LaunchProfileState_CREATE_IN_PROGRESS = LaunchProfileState' "CREATE_IN_PROGRESS"

pattern LaunchProfileState_DELETED :: LaunchProfileState
pattern LaunchProfileState_DELETED = LaunchProfileState' "DELETED"

pattern LaunchProfileState_DELETE_FAILED :: LaunchProfileState
pattern LaunchProfileState_DELETE_FAILED = LaunchProfileState' "DELETE_FAILED"

pattern LaunchProfileState_DELETE_IN_PROGRESS :: LaunchProfileState
pattern LaunchProfileState_DELETE_IN_PROGRESS = LaunchProfileState' "DELETE_IN_PROGRESS"

pattern LaunchProfileState_READY :: LaunchProfileState
pattern LaunchProfileState_READY = LaunchProfileState' "READY"

pattern LaunchProfileState_UPDATE_FAILED :: LaunchProfileState
pattern LaunchProfileState_UPDATE_FAILED = LaunchProfileState' "UPDATE_FAILED"

pattern LaunchProfileState_UPDATE_IN_PROGRESS :: LaunchProfileState
pattern LaunchProfileState_UPDATE_IN_PROGRESS = LaunchProfileState' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  LaunchProfileState_CREATE_FAILED,
  LaunchProfileState_CREATE_IN_PROGRESS,
  LaunchProfileState_DELETED,
  LaunchProfileState_DELETE_FAILED,
  LaunchProfileState_DELETE_IN_PROGRESS,
  LaunchProfileState_READY,
  LaunchProfileState_UPDATE_FAILED,
  LaunchProfileState_UPDATE_IN_PROGRESS,
  LaunchProfileState'
  #-}
