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
-- Module      : Amazonka.LexV2Models.Types.BotRecommendationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotRecommendationStatus
  ( BotRecommendationStatus
      ( ..,
        BotRecommendationStatus_Available,
        BotRecommendationStatus_Deleted,
        BotRecommendationStatus_Deleting,
        BotRecommendationStatus_Downloading,
        BotRecommendationStatus_Failed,
        BotRecommendationStatus_Processing,
        BotRecommendationStatus_Stopped,
        BotRecommendationStatus_Stopping,
        BotRecommendationStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BotRecommendationStatus = BotRecommendationStatus'
  { fromBotRecommendationStatus ::
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

pattern BotRecommendationStatus_Available :: BotRecommendationStatus
pattern BotRecommendationStatus_Available = BotRecommendationStatus' "Available"

pattern BotRecommendationStatus_Deleted :: BotRecommendationStatus
pattern BotRecommendationStatus_Deleted = BotRecommendationStatus' "Deleted"

pattern BotRecommendationStatus_Deleting :: BotRecommendationStatus
pattern BotRecommendationStatus_Deleting = BotRecommendationStatus' "Deleting"

pattern BotRecommendationStatus_Downloading :: BotRecommendationStatus
pattern BotRecommendationStatus_Downloading = BotRecommendationStatus' "Downloading"

pattern BotRecommendationStatus_Failed :: BotRecommendationStatus
pattern BotRecommendationStatus_Failed = BotRecommendationStatus' "Failed"

pattern BotRecommendationStatus_Processing :: BotRecommendationStatus
pattern BotRecommendationStatus_Processing = BotRecommendationStatus' "Processing"

pattern BotRecommendationStatus_Stopped :: BotRecommendationStatus
pattern BotRecommendationStatus_Stopped = BotRecommendationStatus' "Stopped"

pattern BotRecommendationStatus_Stopping :: BotRecommendationStatus
pattern BotRecommendationStatus_Stopping = BotRecommendationStatus' "Stopping"

pattern BotRecommendationStatus_Updating :: BotRecommendationStatus
pattern BotRecommendationStatus_Updating = BotRecommendationStatus' "Updating"

{-# COMPLETE
  BotRecommendationStatus_Available,
  BotRecommendationStatus_Deleted,
  BotRecommendationStatus_Deleting,
  BotRecommendationStatus_Downloading,
  BotRecommendationStatus_Failed,
  BotRecommendationStatus_Processing,
  BotRecommendationStatus_Stopped,
  BotRecommendationStatus_Stopping,
  BotRecommendationStatus_Updating,
  BotRecommendationStatus'
  #-}
