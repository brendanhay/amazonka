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
-- Module      : Amazonka.Glue.Types.PartitionIndexStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PartitionIndexStatus
  ( PartitionIndexStatus
      ( ..,
        PartitionIndexStatus_ACTIVE,
        PartitionIndexStatus_CREATING,
        PartitionIndexStatus_DELETING,
        PartitionIndexStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PartitionIndexStatus = PartitionIndexStatus'
  { fromPartitionIndexStatus ::
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
