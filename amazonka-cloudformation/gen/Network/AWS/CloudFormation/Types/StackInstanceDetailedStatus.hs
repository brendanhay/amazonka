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
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
  ( StackInstanceDetailedStatus
      ( ..,
        StackInstanceDetailedStatus_CANCELLED,
        StackInstanceDetailedStatus_FAILED,
        StackInstanceDetailedStatus_INOPERABLE,
        StackInstanceDetailedStatus_PENDING,
        StackInstanceDetailedStatus_RUNNING,
        StackInstanceDetailedStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StackInstanceDetailedStatus = StackInstanceDetailedStatus'
  { fromStackInstanceDetailedStatus ::
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

pattern StackInstanceDetailedStatus_CANCELLED :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatus_CANCELLED = StackInstanceDetailedStatus' "CANCELLED"

pattern StackInstanceDetailedStatus_FAILED :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatus_FAILED = StackInstanceDetailedStatus' "FAILED"

pattern StackInstanceDetailedStatus_INOPERABLE :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatus_INOPERABLE = StackInstanceDetailedStatus' "INOPERABLE"

pattern StackInstanceDetailedStatus_PENDING :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatus_PENDING = StackInstanceDetailedStatus' "PENDING"

pattern StackInstanceDetailedStatus_RUNNING :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatus_RUNNING = StackInstanceDetailedStatus' "RUNNING"

pattern StackInstanceDetailedStatus_SUCCEEDED :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatus_SUCCEEDED = StackInstanceDetailedStatus' "SUCCEEDED"

{-# COMPLETE
  StackInstanceDetailedStatus_CANCELLED,
  StackInstanceDetailedStatus_FAILED,
  StackInstanceDetailedStatus_INOPERABLE,
  StackInstanceDetailedStatus_PENDING,
  StackInstanceDetailedStatus_RUNNING,
  StackInstanceDetailedStatus_SUCCEEDED,
  StackInstanceDetailedStatus'
  #-}
