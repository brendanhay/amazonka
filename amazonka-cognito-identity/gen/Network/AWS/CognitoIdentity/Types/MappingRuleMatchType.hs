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

import qualified Network.AWS.Core as Core

newtype MappingRuleMatchType = MappingRuleMatchType'
  { fromMappingRuleMatchType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
