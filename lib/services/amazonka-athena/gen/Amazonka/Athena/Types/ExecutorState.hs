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
-- Module      : Amazonka.Athena.Types.ExecutorState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ExecutorState
  ( ExecutorState
      ( ..,
        ExecutorState_CREATED,
        ExecutorState_CREATING,
        ExecutorState_FAILED,
        ExecutorState_REGISTERED,
        ExecutorState_TERMINATED,
        ExecutorState_TERMINATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExecutorState = ExecutorState'
  { fromExecutorState ::
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

pattern ExecutorState_CREATED :: ExecutorState
pattern ExecutorState_CREATED = ExecutorState' "CREATED"

pattern ExecutorState_CREATING :: ExecutorState
pattern ExecutorState_CREATING = ExecutorState' "CREATING"

pattern ExecutorState_FAILED :: ExecutorState
pattern ExecutorState_FAILED = ExecutorState' "FAILED"

pattern ExecutorState_REGISTERED :: ExecutorState
pattern ExecutorState_REGISTERED = ExecutorState' "REGISTERED"

pattern ExecutorState_TERMINATED :: ExecutorState
pattern ExecutorState_TERMINATED = ExecutorState' "TERMINATED"

pattern ExecutorState_TERMINATING :: ExecutorState
pattern ExecutorState_TERMINATING = ExecutorState' "TERMINATING"

{-# COMPLETE
  ExecutorState_CREATED,
  ExecutorState_CREATING,
  ExecutorState_FAILED,
  ExecutorState_REGISTERED,
  ExecutorState_TERMINATED,
  ExecutorState_TERMINATING,
  ExecutorState'
  #-}
