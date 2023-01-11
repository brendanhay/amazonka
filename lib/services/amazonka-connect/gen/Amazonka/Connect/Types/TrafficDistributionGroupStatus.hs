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
-- Module      : Amazonka.Connect.Types.TrafficDistributionGroupStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TrafficDistributionGroupStatus
  ( TrafficDistributionGroupStatus
      ( ..,
        TrafficDistributionGroupStatus_ACTIVE,
        TrafficDistributionGroupStatus_CREATION_FAILED,
        TrafficDistributionGroupStatus_CREATION_IN_PROGRESS,
        TrafficDistributionGroupStatus_DELETION_FAILED,
        TrafficDistributionGroupStatus_PENDING_DELETION,
        TrafficDistributionGroupStatus_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrafficDistributionGroupStatus = TrafficDistributionGroupStatus'
  { fromTrafficDistributionGroupStatus ::
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

pattern TrafficDistributionGroupStatus_ACTIVE :: TrafficDistributionGroupStatus
pattern TrafficDistributionGroupStatus_ACTIVE = TrafficDistributionGroupStatus' "ACTIVE"

pattern TrafficDistributionGroupStatus_CREATION_FAILED :: TrafficDistributionGroupStatus
pattern TrafficDistributionGroupStatus_CREATION_FAILED = TrafficDistributionGroupStatus' "CREATION_FAILED"

pattern TrafficDistributionGroupStatus_CREATION_IN_PROGRESS :: TrafficDistributionGroupStatus
pattern TrafficDistributionGroupStatus_CREATION_IN_PROGRESS = TrafficDistributionGroupStatus' "CREATION_IN_PROGRESS"

pattern TrafficDistributionGroupStatus_DELETION_FAILED :: TrafficDistributionGroupStatus
pattern TrafficDistributionGroupStatus_DELETION_FAILED = TrafficDistributionGroupStatus' "DELETION_FAILED"

pattern TrafficDistributionGroupStatus_PENDING_DELETION :: TrafficDistributionGroupStatus
pattern TrafficDistributionGroupStatus_PENDING_DELETION = TrafficDistributionGroupStatus' "PENDING_DELETION"

pattern TrafficDistributionGroupStatus_UPDATE_IN_PROGRESS :: TrafficDistributionGroupStatus
pattern TrafficDistributionGroupStatus_UPDATE_IN_PROGRESS = TrafficDistributionGroupStatus' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  TrafficDistributionGroupStatus_ACTIVE,
  TrafficDistributionGroupStatus_CREATION_FAILED,
  TrafficDistributionGroupStatus_CREATION_IN_PROGRESS,
  TrafficDistributionGroupStatus_DELETION_FAILED,
  TrafficDistributionGroupStatus_PENDING_DELETION,
  TrafficDistributionGroupStatus_UPDATE_IN_PROGRESS,
  TrafficDistributionGroupStatus'
  #-}
