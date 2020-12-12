{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
  ( MappingRuleMatchType
      ( MappingRuleMatchType',
        Contains,
        Equals,
        NotEqual,
        StartsWith
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MappingRuleMatchType = MappingRuleMatchType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Contains :: MappingRuleMatchType
pattern Contains = MappingRuleMatchType' "Contains"

pattern Equals :: MappingRuleMatchType
pattern Equals = MappingRuleMatchType' "Equals"

pattern NotEqual :: MappingRuleMatchType
pattern NotEqual = MappingRuleMatchType' "NotEqual"

pattern StartsWith :: MappingRuleMatchType
pattern StartsWith = MappingRuleMatchType' "StartsWith"

{-# COMPLETE
  Contains,
  Equals,
  NotEqual,
  StartsWith,
  MappingRuleMatchType'
  #-}
