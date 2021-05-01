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
-- Module      : Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
  ( MappingRuleMatchType
      ( ..,
        MappingRuleMatchType_Contains,
        MappingRuleMatchType_Equals,
        MappingRuleMatchType_NotEqual,
        MappingRuleMatchType_StartsWith
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MappingRuleMatchType = MappingRuleMatchType'
  { fromMappingRuleMatchType ::
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

pattern MappingRuleMatchType_Contains :: MappingRuleMatchType
pattern MappingRuleMatchType_Contains = MappingRuleMatchType' "Contains"

pattern MappingRuleMatchType_Equals :: MappingRuleMatchType
pattern MappingRuleMatchType_Equals = MappingRuleMatchType' "Equals"

pattern MappingRuleMatchType_NotEqual :: MappingRuleMatchType
pattern MappingRuleMatchType_NotEqual = MappingRuleMatchType' "NotEqual"

pattern MappingRuleMatchType_StartsWith :: MappingRuleMatchType
pattern MappingRuleMatchType_StartsWith = MappingRuleMatchType' "StartsWith"

{-# COMPLETE
  MappingRuleMatchType_Contains,
  MappingRuleMatchType_Equals,
  MappingRuleMatchType_NotEqual,
  MappingRuleMatchType_StartsWith,
  MappingRuleMatchType'
  #-}
