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
-- Module      : Amazonka.NetworkFirewall.Types.RuleOrder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.RuleOrder
  ( RuleOrder
      ( ..,
        RuleOrder_DEFAULT_ACTION_ORDER,
        RuleOrder_STRICT_ORDER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RuleOrder = RuleOrder'
  { fromRuleOrder ::
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

pattern RuleOrder_DEFAULT_ACTION_ORDER :: RuleOrder
pattern RuleOrder_DEFAULT_ACTION_ORDER = RuleOrder' "DEFAULT_ACTION_ORDER"

pattern RuleOrder_STRICT_ORDER :: RuleOrder
pattern RuleOrder_STRICT_ORDER = RuleOrder' "STRICT_ORDER"

{-# COMPLETE
  RuleOrder_DEFAULT_ACTION_ORDER,
  RuleOrder_STRICT_ORDER,
  RuleOrder'
  #-}
