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
-- Module      : Network.AWS.CloudFormation.Types.StackDriftStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftStatus
  ( StackDriftStatus
      ( ..,
        StackDriftStatus_DRIFTED,
        StackDriftStatus_IN_SYNC,
        StackDriftStatus_NOT_CHECKED,
        StackDriftStatus_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StackDriftStatus = StackDriftStatus'
  { fromStackDriftStatus ::
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

pattern StackDriftStatus_DRIFTED :: StackDriftStatus
pattern StackDriftStatus_DRIFTED = StackDriftStatus' "DRIFTED"

pattern StackDriftStatus_IN_SYNC :: StackDriftStatus
pattern StackDriftStatus_IN_SYNC = StackDriftStatus' "IN_SYNC"

pattern StackDriftStatus_NOT_CHECKED :: StackDriftStatus
pattern StackDriftStatus_NOT_CHECKED = StackDriftStatus' "NOT_CHECKED"

pattern StackDriftStatus_UNKNOWN :: StackDriftStatus
pattern StackDriftStatus_UNKNOWN = StackDriftStatus' "UNKNOWN"

{-# COMPLETE
  StackDriftStatus_DRIFTED,
  StackDriftStatus_IN_SYNC,
  StackDriftStatus_NOT_CHECKED,
  StackDriftStatus_UNKNOWN,
  StackDriftStatus'
  #-}
