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
-- Module      : Network.AWS.Pinpoint.Types.CampaignStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype CampaignStatus = CampaignStatus'
  { fromCampaignStatus ::
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
