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
-- Module      : Amazonka.Config.Types.ConformancePackState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackState
  ( ConformancePackState
      ( ..,
        ConformancePackState_CREATE_COMPLETE,
        ConformancePackState_CREATE_FAILED,
        ConformancePackState_CREATE_IN_PROGRESS,
        ConformancePackState_DELETE_FAILED,
        ConformancePackState_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConformancePackState = ConformancePackState'
  { fromConformancePackState ::
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

pattern ConformancePackState_CREATE_COMPLETE :: ConformancePackState
pattern ConformancePackState_CREATE_COMPLETE = ConformancePackState' "CREATE_COMPLETE"

pattern ConformancePackState_CREATE_FAILED :: ConformancePackState
pattern ConformancePackState_CREATE_FAILED = ConformancePackState' "CREATE_FAILED"

pattern ConformancePackState_CREATE_IN_PROGRESS :: ConformancePackState
pattern ConformancePackState_CREATE_IN_PROGRESS = ConformancePackState' "CREATE_IN_PROGRESS"

pattern ConformancePackState_DELETE_FAILED :: ConformancePackState
pattern ConformancePackState_DELETE_FAILED = ConformancePackState' "DELETE_FAILED"

pattern ConformancePackState_DELETE_IN_PROGRESS :: ConformancePackState
pattern ConformancePackState_DELETE_IN_PROGRESS = ConformancePackState' "DELETE_IN_PROGRESS"

{-# COMPLETE
  ConformancePackState_CREATE_COMPLETE,
  ConformancePackState_CREATE_FAILED,
  ConformancePackState_CREATE_IN_PROGRESS,
  ConformancePackState_DELETE_FAILED,
  ConformancePackState_DELETE_IN_PROGRESS,
  ConformancePackState'
  #-}
