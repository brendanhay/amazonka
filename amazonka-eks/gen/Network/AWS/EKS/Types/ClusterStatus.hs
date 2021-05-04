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
-- Module      : Network.AWS.EKS.Types.ClusterStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.ClusterStatus
  ( ClusterStatus
      ( ..,
        ClusterStatus_ACTIVE,
        ClusterStatus_CREATING,
        ClusterStatus_DELETING,
        ClusterStatus_FAILED,
        ClusterStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ClusterStatus = ClusterStatus'
  { fromClusterStatus ::
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

pattern ClusterStatus_ACTIVE :: ClusterStatus
pattern ClusterStatus_ACTIVE = ClusterStatus' "ACTIVE"

pattern ClusterStatus_CREATING :: ClusterStatus
pattern ClusterStatus_CREATING = ClusterStatus' "CREATING"

pattern ClusterStatus_DELETING :: ClusterStatus
pattern ClusterStatus_DELETING = ClusterStatus' "DELETING"

pattern ClusterStatus_FAILED :: ClusterStatus
pattern ClusterStatus_FAILED = ClusterStatus' "FAILED"

pattern ClusterStatus_UPDATING :: ClusterStatus
pattern ClusterStatus_UPDATING = ClusterStatus' "UPDATING"

{-# COMPLETE
  ClusterStatus_ACTIVE,
  ClusterStatus_CREATING,
  ClusterStatus_DELETING,
  ClusterStatus_FAILED,
  ClusterStatus_UPDATING,
  ClusterStatus'
  #-}
