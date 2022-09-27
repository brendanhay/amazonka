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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype LaunchStopDesiredState = LaunchStopDesiredState'
  { fromLaunchStopDesiredState ::
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

pattern LaunchStopDesiredState_CANCELLED :: LaunchStopDesiredState
pattern LaunchStopDesiredState_CANCELLED = LaunchStopDesiredState' "CANCELLED"

pattern LaunchStopDesiredState_COMPLETED :: LaunchStopDesiredState
pattern LaunchStopDesiredState_COMPLETED = LaunchStopDesiredState' "COMPLETED"

{-# COMPLETE
  LaunchStopDesiredState_CANCELLED,
  LaunchStopDesiredState_COMPLETED,
  LaunchStopDesiredState'
  #-}
