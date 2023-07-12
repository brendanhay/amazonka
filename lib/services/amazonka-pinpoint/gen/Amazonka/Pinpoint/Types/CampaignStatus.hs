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
-- Module      : Amazonka.Pinpoint.Types.CampaignStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CampaignStatus
  ( CampaignStatus
      ( ..,
        CampaignStatus_COMPLETED,
        CampaignStatus_DELETED,
        CampaignStatus_EXECUTING,
        CampaignStatus_INVALID,
        CampaignStatus_PAUSED,
        CampaignStatus_PENDING_NEXT_RUN,
        CampaignStatus_SCHEDULED
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

pattern CampaignStatus_COMPLETED :: CampaignStatus
pattern CampaignStatus_COMPLETED = CampaignStatus' "COMPLETED"

pattern CampaignStatus_DELETED :: CampaignStatus
pattern CampaignStatus_DELETED = CampaignStatus' "DELETED"

pattern CampaignStatus_EXECUTING :: CampaignStatus
pattern CampaignStatus_EXECUTING = CampaignStatus' "EXECUTING"

pattern CampaignStatus_INVALID :: CampaignStatus
pattern CampaignStatus_INVALID = CampaignStatus' "INVALID"

pattern CampaignStatus_PAUSED :: CampaignStatus
pattern CampaignStatus_PAUSED = CampaignStatus' "PAUSED"

pattern CampaignStatus_PENDING_NEXT_RUN :: CampaignStatus
pattern CampaignStatus_PENDING_NEXT_RUN = CampaignStatus' "PENDING_NEXT_RUN"

pattern CampaignStatus_SCHEDULED :: CampaignStatus
pattern CampaignStatus_SCHEDULED = CampaignStatus' "SCHEDULED"

{-# COMPLETE
  CampaignStatus_COMPLETED,
  CampaignStatus_DELETED,
  CampaignStatus_EXECUTING,
  CampaignStatus_INVALID,
  CampaignStatus_PAUSED,
  CampaignStatus_PENDING_NEXT_RUN,
  CampaignStatus_SCHEDULED,
  CampaignStatus'
  #-}
