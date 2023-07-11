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
-- Module      : Amazonka.WorkSpaces.Types.RunningMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.RunningMode
  ( RunningMode
      ( ..,
        RunningMode_ALWAYS_ON,
        RunningMode_AUTO_STOP,
        RunningMode_MANUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RunningMode = RunningMode'
  { fromRunningMode ::
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

pattern RunningMode_ALWAYS_ON :: RunningMode
pattern RunningMode_ALWAYS_ON = RunningMode' "ALWAYS_ON"

pattern RunningMode_AUTO_STOP :: RunningMode
pattern RunningMode_AUTO_STOP = RunningMode' "AUTO_STOP"

pattern RunningMode_MANUAL :: RunningMode
pattern RunningMode_MANUAL = RunningMode' "MANUAL"

{-# COMPLETE
  RunningMode_ALWAYS_ON,
  RunningMode_AUTO_STOP,
  RunningMode_MANUAL,
  RunningMode'
  #-}
