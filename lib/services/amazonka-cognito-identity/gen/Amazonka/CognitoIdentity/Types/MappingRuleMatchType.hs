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
-- Module      : Amazonka.CognitoIdentity.Types.MappingRuleMatchType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.MappingRuleMatchType
  ( MappingRuleMatchType
      ( ..,
        MappingRuleMatchType_Contains,
        MappingRuleMatchType_Equals,
        MappingRuleMatchType_NotEqual,
        MappingRuleMatchType_StartsWith
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MappingRuleMatchType = MappingRuleMatchType'
  { fromMappingRuleMatchType ::
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
