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
-- Module      : Amazonka.EKS.Types.ClusterStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.ClusterStatus
  ( ClusterStatus
      ( ..,
        ClusterStatus_ACTIVE,
        ClusterStatus_CREATING,
        ClusterStatus_DELETING,
        ClusterStatus_FAILED,
        ClusterStatus_PENDING,
        ClusterStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClusterStatus = ClusterStatus'
  { fromClusterStatus ::
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

pattern ClusterStatus_ACTIVE :: ClusterStatus
pattern ClusterStatus_ACTIVE = ClusterStatus' "ACTIVE"

pattern ClusterStatus_CREATING :: ClusterStatus
pattern ClusterStatus_CREATING = ClusterStatus' "CREATING"

pattern ClusterStatus_DELETING :: ClusterStatus
pattern ClusterStatus_DELETING = ClusterStatus' "DELETING"

pattern ClusterStatus_FAILED :: ClusterStatus
pattern ClusterStatus_FAILED = ClusterStatus' "FAILED"

pattern ClusterStatus_PENDING :: ClusterStatus
pattern ClusterStatus_PENDING = ClusterStatus' "PENDING"

pattern ClusterStatus_UPDATING :: ClusterStatus
pattern ClusterStatus_UPDATING = ClusterStatus' "UPDATING"

{-# COMPLETE
  ClusterStatus_ACTIVE,
  ClusterStatus_CREATING,
  ClusterStatus_DELETING,
  ClusterStatus_FAILED,
  ClusterStatus_PENDING,
  ClusterStatus_UPDATING,
  ClusterStatus'
  #-}
