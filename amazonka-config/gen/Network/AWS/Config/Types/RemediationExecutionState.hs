{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionState
  ( RemediationExecutionState
      ( ..,
        RemediationExecutionState_FAILED,
        RemediationExecutionState_IN_PROGRESS,
        RemediationExecutionState_QUEUED,
        RemediationExecutionState_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RemediationExecutionState = RemediationExecutionState'
  { fromRemediationExecutionState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern RemediationExecutionState_FAILED :: RemediationExecutionState
pattern RemediationExecutionState_FAILED = RemediationExecutionState' "FAILED"

pattern RemediationExecutionState_IN_PROGRESS :: RemediationExecutionState
pattern RemediationExecutionState_IN_PROGRESS = RemediationExecutionState' "IN_PROGRESS"

pattern RemediationExecutionState_QUEUED :: RemediationExecutionState
pattern RemediationExecutionState_QUEUED = RemediationExecutionState' "QUEUED"

pattern RemediationExecutionState_SUCCEEDED :: RemediationExecutionState
pattern RemediationExecutionState_SUCCEEDED = RemediationExecutionState' "SUCCEEDED"

{-# COMPLETE
  RemediationExecutionState_FAILED,
  RemediationExecutionState_IN_PROGRESS,
  RemediationExecutionState_QUEUED,
  RemediationExecutionState_SUCCEEDED,
  RemediationExecutionState'
  #-}
