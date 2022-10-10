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
-- Module      : Amazonka.IoTFleetWise.Types.UpdateCampaignAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.UpdateCampaignAction
  ( UpdateCampaignAction
      ( ..,
        UpdateCampaignAction_APPROVE,
        UpdateCampaignAction_RESUME,
        UpdateCampaignAction_SUSPEND,
        UpdateCampaignAction_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UpdateCampaignAction = UpdateCampaignAction'
  { fromUpdateCampaignAction ::
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

pattern UpdateCampaignAction_APPROVE :: UpdateCampaignAction
pattern UpdateCampaignAction_APPROVE = UpdateCampaignAction' "APPROVE"

pattern UpdateCampaignAction_RESUME :: UpdateCampaignAction
pattern UpdateCampaignAction_RESUME = UpdateCampaignAction' "RESUME"

pattern UpdateCampaignAction_SUSPEND :: UpdateCampaignAction
pattern UpdateCampaignAction_SUSPEND = UpdateCampaignAction' "SUSPEND"

pattern UpdateCampaignAction_UPDATE :: UpdateCampaignAction
pattern UpdateCampaignAction_UPDATE = UpdateCampaignAction' "UPDATE"

{-# COMPLETE
  UpdateCampaignAction_APPROVE,
  UpdateCampaignAction_RESUME,
  UpdateCampaignAction_SUSPEND,
  UpdateCampaignAction_UPDATE,
  UpdateCampaignAction'
  #-}
