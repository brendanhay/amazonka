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
-- Module      : Network.AWS.EC2.Types.BatchState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BatchState
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype BatchState = BatchState'
  { fromBatchState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
