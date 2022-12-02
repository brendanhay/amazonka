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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.AccountLimitName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.AccountLimitName
  ( AccountLimitName
      ( ..,
        AccountLimitName_CONFIGURATION_SETS,
        AccountLimitName_OPT_OUT_LISTS,
        AccountLimitName_PHONE_NUMBERS,
        AccountLimitName_POOLS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccountLimitName = AccountLimitName'
  { fromAccountLimitName ::
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

pattern AccountLimitName_CONFIGURATION_SETS :: AccountLimitName
pattern AccountLimitName_CONFIGURATION_SETS = AccountLimitName' "CONFIGURATION_SETS"

pattern AccountLimitName_OPT_OUT_LISTS :: AccountLimitName
pattern AccountLimitName_OPT_OUT_LISTS = AccountLimitName' "OPT_OUT_LISTS"

pattern AccountLimitName_PHONE_NUMBERS :: AccountLimitName
pattern AccountLimitName_PHONE_NUMBERS = AccountLimitName' "PHONE_NUMBERS"

pattern AccountLimitName_POOLS :: AccountLimitName
pattern AccountLimitName_POOLS = AccountLimitName' "POOLS"

{-# COMPLETE
  AccountLimitName_CONFIGURATION_SETS,
  AccountLimitName_OPT_OUT_LISTS,
  AccountLimitName_PHONE_NUMBERS,
  AccountLimitName_POOLS,
  AccountLimitName'
  #-}
