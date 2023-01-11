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
-- Module      : Amazonka.LexV2Models.Types.BotAliasStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotAliasStatus
  ( BotAliasStatus
      ( ..,
        BotAliasStatus_Available,
        BotAliasStatus_Creating,
        BotAliasStatus_Deleting,
        BotAliasStatus_Failed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BotAliasStatus = BotAliasStatus'
  { fromBotAliasStatus ::
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

pattern BotAliasStatus_Available :: BotAliasStatus
pattern BotAliasStatus_Available = BotAliasStatus' "Available"

pattern BotAliasStatus_Creating :: BotAliasStatus
pattern BotAliasStatus_Creating = BotAliasStatus' "Creating"

pattern BotAliasStatus_Deleting :: BotAliasStatus
pattern BotAliasStatus_Deleting = BotAliasStatus' "Deleting"

pattern BotAliasStatus_Failed :: BotAliasStatus
pattern BotAliasStatus_Failed = BotAliasStatus' "Failed"

{-# COMPLETE
  BotAliasStatus_Available,
  BotAliasStatus_Creating,
  BotAliasStatus_Deleting,
  BotAliasStatus_Failed,
  BotAliasStatus'
  #-}
