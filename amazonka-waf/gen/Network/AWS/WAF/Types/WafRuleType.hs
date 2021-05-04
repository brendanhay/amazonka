{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.WafRuleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WafRuleType
  ( WafRuleType
      ( ..,
        WafRuleType_GROUP,
        WafRuleType_RATE_BASED,
        WafRuleType_REGULAR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype WafRuleType = WafRuleType'
  { fromWafRuleType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern WafRuleType_GROUP :: WafRuleType
pattern WafRuleType_GROUP = WafRuleType' "GROUP"

pattern WafRuleType_RATE_BASED :: WafRuleType
pattern WafRuleType_RATE_BASED = WafRuleType' "RATE_BASED"

pattern WafRuleType_REGULAR :: WafRuleType
pattern WafRuleType_REGULAR = WafRuleType' "REGULAR"

{-# COMPLETE
  WafRuleType_GROUP,
  WafRuleType_RATE_BASED,
  WafRuleType_REGULAR,
  WafRuleType'
  #-}
