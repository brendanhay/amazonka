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
-- Module      : Amazonka.ConnectCampaigns.Types.CampaignState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.CampaignState
  ( CampaignState
      ( ..,
        CampaignState_Failed,
        CampaignState_Initialized,
        CampaignState_Paused,
        CampaignState_Running,
        CampaignState_Stopped
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | State of a campaign
newtype CampaignState = CampaignState'
  { fromCampaignState ::
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

pattern CampaignState_Failed :: CampaignState
pattern CampaignState_Failed = CampaignState' "Failed"

pattern CampaignState_Initialized :: CampaignState
pattern CampaignState_Initialized = CampaignState' "Initialized"

pattern CampaignState_Paused :: CampaignState
pattern CampaignState_Paused = CampaignState' "Paused"

pattern CampaignState_Running :: CampaignState
pattern CampaignState_Running = CampaignState' "Running"

pattern CampaignState_Stopped :: CampaignState
pattern CampaignState_Stopped = CampaignState' "Stopped"

{-# COMPLETE
  CampaignState_Failed,
  CampaignState_Initialized,
  CampaignState_Paused,
  CampaignState_Running,
  CampaignState_Stopped,
  CampaignState'
  #-}
