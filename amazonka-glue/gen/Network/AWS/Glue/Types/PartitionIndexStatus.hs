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
-- Module      : Network.AWS.Glue.Types.PartitionIndexStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndexStatus
  ( PartitionIndexStatus
      ( ..,
        PartitionIndexStatus_ACTIVE,
        PartitionIndexStatus_CREATING,
        PartitionIndexStatus_DELETING,
        PartitionIndexStatus_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PartitionIndexStatus = PartitionIndexStatus'
  { fromPartitionIndexStatus ::
      Core.Text
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

pattern PartitionIndexStatus_ACTIVE :: PartitionIndexStatus
pattern PartitionIndexStatus_ACTIVE = PartitionIndexStatus' "ACTIVE"

pattern PartitionIndexStatus_CREATING :: PartitionIndexStatus
pattern PartitionIndexStatus_CREATING = PartitionIndexStatus' "CREATING"

pattern PartitionIndexStatus_DELETING :: PartitionIndexStatus
pattern PartitionIndexStatus_DELETING = PartitionIndexStatus' "DELETING"

pattern PartitionIndexStatus_FAILED :: PartitionIndexStatus
pattern PartitionIndexStatus_FAILED = PartitionIndexStatus' "FAILED"

{-# COMPLETE
  PartitionIndexStatus_ACTIVE,
  PartitionIndexStatus_CREATING,
  PartitionIndexStatus_DELETING,
  PartitionIndexStatus_FAILED,
  PartitionIndexStatus'
  #-}
