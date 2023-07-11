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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UpdateCampaignAction = UpdateCampaignAction'
  { fromUpdateCampaignAction ::
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
