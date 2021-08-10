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
-- Module      : Network.AWS.WAFRegional.Types.PredicateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.PredicateType
  ( PredicateType
      ( ..,
        PredicateType_ByteMatch,
        PredicateType_GeoMatch,
        PredicateType_IPMatch,
        PredicateType_RegexMatch,
        PredicateType_SizeConstraint,
        PredicateType_SqlInjectionMatch,
        PredicateType_XssMatch
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PredicateType = PredicateType'
  { fromPredicateType ::
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

pattern PredicateType_ByteMatch :: PredicateType
pattern PredicateType_ByteMatch = PredicateType' "ByteMatch"

pattern PredicateType_GeoMatch :: PredicateType
pattern PredicateType_GeoMatch = PredicateType' "GeoMatch"

pattern PredicateType_IPMatch :: PredicateType
pattern PredicateType_IPMatch = PredicateType' "IPMatch"

pattern PredicateType_RegexMatch :: PredicateType
pattern PredicateType_RegexMatch = PredicateType' "RegexMatch"

pattern PredicateType_SizeConstraint :: PredicateType
pattern PredicateType_SizeConstraint = PredicateType' "SizeConstraint"

pattern PredicateType_SqlInjectionMatch :: PredicateType
pattern PredicateType_SqlInjectionMatch = PredicateType' "SqlInjectionMatch"

pattern PredicateType_XssMatch :: PredicateType
pattern PredicateType_XssMatch = PredicateType' "XssMatch"

{-# COMPLETE
  PredicateType_ByteMatch,
  PredicateType_GeoMatch,
  PredicateType_IPMatch,
  PredicateType_RegexMatch,
  PredicateType_SizeConstraint,
  PredicateType_SqlInjectionMatch,
  PredicateType_XssMatch,
  PredicateType'
  #-}
