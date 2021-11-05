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
-- Module      : Amazonka.IoTEventsData.Types.CustomerActionName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.CustomerActionName
  ( CustomerActionName
      ( ..,
        CustomerActionName_ACKNOWLEDGE,
        CustomerActionName_DISABLE,
        CustomerActionName_ENABLE,
        CustomerActionName_RESET,
        CustomerActionName_SNOOZE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CustomerActionName = CustomerActionName'
  { fromCustomerActionName ::
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

pattern CustomerActionName_ACKNOWLEDGE :: CustomerActionName
pattern CustomerActionName_ACKNOWLEDGE = CustomerActionName' "ACKNOWLEDGE"

pattern CustomerActionName_DISABLE :: CustomerActionName
pattern CustomerActionName_DISABLE = CustomerActionName' "DISABLE"

pattern CustomerActionName_ENABLE :: CustomerActionName
pattern CustomerActionName_ENABLE = CustomerActionName' "ENABLE"

pattern CustomerActionName_RESET :: CustomerActionName
pattern CustomerActionName_RESET = CustomerActionName' "RESET"

pattern CustomerActionName_SNOOZE :: CustomerActionName
pattern CustomerActionName_SNOOZE = CustomerActionName' "SNOOZE"

{-# COMPLETE
  CustomerActionName_ACKNOWLEDGE,
  CustomerActionName_DISABLE,
  CustomerActionName_ENABLE,
  CustomerActionName_RESET,
  CustomerActionName_SNOOZE,
  CustomerActionName'
  #-}
