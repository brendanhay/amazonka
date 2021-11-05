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
-- Module      : Network.AWS.SSMIncidents.Types.RegionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.RegionStatus
  ( RegionStatus
      ( ..,
        RegionStatus_ACTIVE,
        RegionStatus_CREATING,
        RegionStatus_DELETING,
        RegionStatus_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RegionStatus = RegionStatus'
  { fromRegionStatus ::
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

pattern RegionStatus_ACTIVE :: RegionStatus
pattern RegionStatus_ACTIVE = RegionStatus' "ACTIVE"

pattern RegionStatus_CREATING :: RegionStatus
pattern RegionStatus_CREATING = RegionStatus' "CREATING"

pattern RegionStatus_DELETING :: RegionStatus
pattern RegionStatus_DELETING = RegionStatus' "DELETING"

pattern RegionStatus_FAILED :: RegionStatus
pattern RegionStatus_FAILED = RegionStatus' "FAILED"

{-# COMPLETE
  RegionStatus_ACTIVE,
  RegionStatus_CREATING,
  RegionStatus_DELETING,
  RegionStatus_FAILED,
  RegionStatus'
  #-}
