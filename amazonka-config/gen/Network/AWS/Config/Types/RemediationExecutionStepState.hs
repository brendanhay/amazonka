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
-- Module      : Network.AWS.Config.Types.RemediationExecutionStepState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStepState
  ( RemediationExecutionStepState
      ( ..,
        RemediationExecutionStepState_FAILED,
        RemediationExecutionStepState_PENDING,
        RemediationExecutionStepState_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RemediationExecutionStepState = RemediationExecutionStepState'
  { fromRemediationExecutionStepState ::
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

pattern RemediationExecutionStepState_FAILED :: RemediationExecutionStepState
pattern RemediationExecutionStepState_FAILED = RemediationExecutionStepState' "FAILED"

pattern RemediationExecutionStepState_PENDING :: RemediationExecutionStepState
pattern RemediationExecutionStepState_PENDING = RemediationExecutionStepState' "PENDING"

pattern RemediationExecutionStepState_SUCCEEDED :: RemediationExecutionStepState
pattern RemediationExecutionStepState_SUCCEEDED = RemediationExecutionStepState' "SUCCEEDED"

{-# COMPLETE
  RemediationExecutionStepState_FAILED,
  RemediationExecutionStepState_PENDING,
  RemediationExecutionStepState_SUCCEEDED,
  RemediationExecutionStepState'
  #-}
