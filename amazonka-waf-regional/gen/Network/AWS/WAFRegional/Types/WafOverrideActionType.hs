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
-- Module      : Network.AWS.WAFRegional.Types.WafOverrideActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WafOverrideActionType
  ( WafOverrideActionType
      ( ..,
        WafOverrideActionType_COUNT,
        WafOverrideActionType_NONE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype WafOverrideActionType = WafOverrideActionType'
  { fromWafOverrideActionType ::
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

pattern WafOverrideActionType_COUNT :: WafOverrideActionType
pattern WafOverrideActionType_COUNT = WafOverrideActionType' "COUNT"

pattern WafOverrideActionType_NONE :: WafOverrideActionType
pattern WafOverrideActionType_NONE = WafOverrideActionType' "NONE"

{-# COMPLETE
  WafOverrideActionType_COUNT,
  WafOverrideActionType_NONE,
  WafOverrideActionType'
  #-}
