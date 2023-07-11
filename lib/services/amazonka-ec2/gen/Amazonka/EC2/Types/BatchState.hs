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
-- Module      : Amazonka.EC2.Types.BatchState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.BatchState
  ( BatchState
      ( ..,
        BatchState_Active,
        BatchState_Cancelled,
        BatchState_Cancelled_running,
        BatchState_Cancelled_terminating,
        BatchState_Failed,
        BatchState_Modifying,
        BatchState_Submitted
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype BatchState = BatchState'
  { fromBatchState ::
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

pattern BatchState_Active :: BatchState
pattern BatchState_Active = BatchState' "active"

pattern BatchState_Cancelled :: BatchState
pattern BatchState_Cancelled = BatchState' "cancelled"

pattern BatchState_Cancelled_running :: BatchState
pattern BatchState_Cancelled_running = BatchState' "cancelled_running"

pattern BatchState_Cancelled_terminating :: BatchState
pattern BatchState_Cancelled_terminating = BatchState' "cancelled_terminating"

pattern BatchState_Failed :: BatchState
pattern BatchState_Failed = BatchState' "failed"

pattern BatchState_Modifying :: BatchState
pattern BatchState_Modifying = BatchState' "modifying"

pattern BatchState_Submitted :: BatchState
pattern BatchState_Submitted = BatchState' "submitted"

{-# COMPLETE
  BatchState_Active,
  BatchState_Cancelled,
  BatchState_Cancelled_running,
  BatchState_Cancelled_terminating,
  BatchState_Failed,
  BatchState_Modifying,
  BatchState_Submitted,
  BatchState'
  #-}
