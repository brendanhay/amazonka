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
-- Module      : Amazonka.LexV2Models.Types.BotLocaleStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotLocaleStatus
  ( BotLocaleStatus
      ( ..,
        BotLocaleStatus_Building,
        BotLocaleStatus_Built,
        BotLocaleStatus_Creating,
        BotLocaleStatus_Deleting,
        BotLocaleStatus_Failed,
        BotLocaleStatus_Importing,
        BotLocaleStatus_NotBuilt,
        BotLocaleStatus_Processing,
        BotLocaleStatus_ReadyExpressTesting
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BotLocaleStatus = BotLocaleStatus'
  { fromBotLocaleStatus ::
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

pattern BotLocaleStatus_Building :: BotLocaleStatus
pattern BotLocaleStatus_Building = BotLocaleStatus' "Building"

pattern BotLocaleStatus_Built :: BotLocaleStatus
pattern BotLocaleStatus_Built = BotLocaleStatus' "Built"

pattern BotLocaleStatus_Creating :: BotLocaleStatus
pattern BotLocaleStatus_Creating = BotLocaleStatus' "Creating"

pattern BotLocaleStatus_Deleting :: BotLocaleStatus
pattern BotLocaleStatus_Deleting = BotLocaleStatus' "Deleting"

pattern BotLocaleStatus_Failed :: BotLocaleStatus
pattern BotLocaleStatus_Failed = BotLocaleStatus' "Failed"

pattern BotLocaleStatus_Importing :: BotLocaleStatus
pattern BotLocaleStatus_Importing = BotLocaleStatus' "Importing"

pattern BotLocaleStatus_NotBuilt :: BotLocaleStatus
pattern BotLocaleStatus_NotBuilt = BotLocaleStatus' "NotBuilt"

pattern BotLocaleStatus_Processing :: BotLocaleStatus
pattern BotLocaleStatus_Processing = BotLocaleStatus' "Processing"

pattern BotLocaleStatus_ReadyExpressTesting :: BotLocaleStatus
pattern BotLocaleStatus_ReadyExpressTesting = BotLocaleStatus' "ReadyExpressTesting"

{-# COMPLETE
  BotLocaleStatus_Building,
  BotLocaleStatus_Built,
  BotLocaleStatus_Creating,
  BotLocaleStatus_Deleting,
  BotLocaleStatus_Failed,
  BotLocaleStatus_Importing,
  BotLocaleStatus_NotBuilt,
  BotLocaleStatus_Processing,
  BotLocaleStatus_ReadyExpressTesting,
  BotLocaleStatus'
  #-}
