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
-- Module      : Amazonka.LexV2Models.Types.BotStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotStatus
  ( BotStatus
      ( ..,
        BotStatus_Available,
        BotStatus_Creating,
        BotStatus_Deleting,
        BotStatus_Failed,
        BotStatus_Importing,
        BotStatus_Inactive,
        BotStatus_Versioning
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BotStatus = BotStatus'
  { fromBotStatus ::
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

pattern BotStatus_Available :: BotStatus
pattern BotStatus_Available = BotStatus' "Available"

pattern BotStatus_Creating :: BotStatus
pattern BotStatus_Creating = BotStatus' "Creating"

pattern BotStatus_Deleting :: BotStatus
pattern BotStatus_Deleting = BotStatus' "Deleting"

pattern BotStatus_Failed :: BotStatus
pattern BotStatus_Failed = BotStatus' "Failed"

pattern BotStatus_Importing :: BotStatus
pattern BotStatus_Importing = BotStatus' "Importing"

pattern BotStatus_Inactive :: BotStatus
pattern BotStatus_Inactive = BotStatus' "Inactive"

pattern BotStatus_Versioning :: BotStatus
pattern BotStatus_Versioning = BotStatus' "Versioning"

{-# COMPLETE
  BotStatus_Available,
  BotStatus_Creating,
  BotStatus_Deleting,
  BotStatus_Failed,
  BotStatus_Importing,
  BotStatus_Inactive,
  BotStatus_Versioning,
  BotStatus'
  #-}
