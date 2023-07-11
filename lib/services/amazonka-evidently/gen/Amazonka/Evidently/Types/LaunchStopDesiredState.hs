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
-- Module      : Amazonka.Evidently.Types.LaunchStopDesiredState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.LaunchStopDesiredState
  ( LaunchStopDesiredState
      ( ..,
        LaunchStopDesiredState_CANCELLED,
        LaunchStopDesiredState_COMPLETED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchStopDesiredState = LaunchStopDesiredState'
  { fromLaunchStopDesiredState ::
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

pattern LaunchStopDesiredState_CANCELLED :: LaunchStopDesiredState
pattern LaunchStopDesiredState_CANCELLED = LaunchStopDesiredState' "CANCELLED"

pattern LaunchStopDesiredState_COMPLETED :: LaunchStopDesiredState
pattern LaunchStopDesiredState_COMPLETED = LaunchStopDesiredState' "COMPLETED"

{-# COMPLETE
  LaunchStopDesiredState_CANCELLED,
  LaunchStopDesiredState_COMPLETED,
  LaunchStopDesiredState'
  #-}
