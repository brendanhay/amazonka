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

import qualified Network.AWS.Prelude as Prelude

newtype StackInstanceDetailedStatus = StackInstanceDetailedStatus'
  { fromStackInstanceDetailedStatus ::
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
