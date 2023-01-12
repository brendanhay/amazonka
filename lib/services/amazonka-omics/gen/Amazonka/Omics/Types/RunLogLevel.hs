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
-- Module      : Amazonka.Omics.Types.RunLogLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.RunLogLevel
  ( RunLogLevel
      ( ..,
        RunLogLevel_ALL,
        RunLogLevel_ERROR,
        RunLogLevel_FATAL,
        RunLogLevel_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RunLogLevel = RunLogLevel'
  { fromRunLogLevel ::
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

pattern RunLogLevel_ALL :: RunLogLevel
pattern RunLogLevel_ALL = RunLogLevel' "ALL"

pattern RunLogLevel_ERROR :: RunLogLevel
pattern RunLogLevel_ERROR = RunLogLevel' "ERROR"

pattern RunLogLevel_FATAL :: RunLogLevel
pattern RunLogLevel_FATAL = RunLogLevel' "FATAL"

pattern RunLogLevel_OFF :: RunLogLevel
pattern RunLogLevel_OFF = RunLogLevel' "OFF"

{-# COMPLETE
  RunLogLevel_ALL,
  RunLogLevel_ERROR,
  RunLogLevel_FATAL,
  RunLogLevel_OFF,
  RunLogLevel'
  #-}
