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
-- Module      : Amazonka.IoTFleetWise.Types.CampaignStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.CampaignStatus
  ( CampaignStatus
      ( ..,
        CampaignStatus_CREATING,
        CampaignStatus_RUNNING,
        CampaignStatus_SUSPENDED,
        CampaignStatus_WAITING_FOR_APPROVAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CampaignStatus = CampaignStatus'
  { fromCampaignStatus ::
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

pattern CampaignStatus_CREATING :: CampaignStatus
pattern CampaignStatus_CREATING = CampaignStatus' "CREATING"

pattern CampaignStatus_RUNNING :: CampaignStatus
pattern CampaignStatus_RUNNING = CampaignStatus' "RUNNING"

pattern CampaignStatus_SUSPENDED :: CampaignStatus
pattern CampaignStatus_SUSPENDED = CampaignStatus' "SUSPENDED"

pattern CampaignStatus_WAITING_FOR_APPROVAL :: CampaignStatus
pattern CampaignStatus_WAITING_FOR_APPROVAL = CampaignStatus' "WAITING_FOR_APPROVAL"

{-# COMPLETE
  CampaignStatus_CREATING,
  CampaignStatus_RUNNING,
  CampaignStatus_SUSPENDED,
  CampaignStatus_WAITING_FOR_APPROVAL,
  CampaignStatus'
  #-}
