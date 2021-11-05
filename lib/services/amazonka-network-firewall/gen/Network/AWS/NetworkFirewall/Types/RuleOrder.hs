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
-- Module      : Network.AWS.NetworkFirewall.Types.RuleOrder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkFirewall.Types.RuleOrder
  ( RuleOrder
      ( ..,
        RuleOrder_DEFAULT_ACTION_ORDER,
        RuleOrder_STRICT_ORDER
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RuleOrder = RuleOrder'
  { fromRuleOrder ::
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

pattern RuleOrder_DEFAULT_ACTION_ORDER :: RuleOrder
pattern RuleOrder_DEFAULT_ACTION_ORDER = RuleOrder' "DEFAULT_ACTION_ORDER"

pattern RuleOrder_STRICT_ORDER :: RuleOrder
pattern RuleOrder_STRICT_ORDER = RuleOrder' "STRICT_ORDER"

{-# COMPLETE
  RuleOrder_DEFAULT_ACTION_ORDER,
  RuleOrder_STRICT_ORDER,
  RuleOrder'
  #-}
