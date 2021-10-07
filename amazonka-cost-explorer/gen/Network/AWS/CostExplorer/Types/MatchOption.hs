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
-- Module      : Network.AWS.CostExplorer.Types.MatchOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MatchOption
  ( MatchOption
      ( ..,
        MatchOption_ABSENT,
        MatchOption_CASE_INSENSITIVE,
        MatchOption_CASE_SENSITIVE,
        MatchOption_CONTAINS,
        MatchOption_ENDS_WITH,
        MatchOption_EQUALS,
        MatchOption_STARTS_WITH
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype MatchOption = MatchOption'
  { fromMatchOption ::
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

pattern MatchOption_ABSENT :: MatchOption
pattern MatchOption_ABSENT = MatchOption' "ABSENT"

pattern MatchOption_CASE_INSENSITIVE :: MatchOption
pattern MatchOption_CASE_INSENSITIVE = MatchOption' "CASE_INSENSITIVE"

pattern MatchOption_CASE_SENSITIVE :: MatchOption
pattern MatchOption_CASE_SENSITIVE = MatchOption' "CASE_SENSITIVE"

pattern MatchOption_CONTAINS :: MatchOption
pattern MatchOption_CONTAINS = MatchOption' "CONTAINS"

pattern MatchOption_ENDS_WITH :: MatchOption
pattern MatchOption_ENDS_WITH = MatchOption' "ENDS_WITH"

pattern MatchOption_EQUALS :: MatchOption
pattern MatchOption_EQUALS = MatchOption' "EQUALS"

pattern MatchOption_STARTS_WITH :: MatchOption
pattern MatchOption_STARTS_WITH = MatchOption' "STARTS_WITH"

{-# COMPLETE
  MatchOption_ABSENT,
  MatchOption_CASE_INSENSITIVE,
  MatchOption_CASE_SENSITIVE,
  MatchOption_CONTAINS,
  MatchOption_ENDS_WITH,
  MatchOption_EQUALS,
  MatchOption_STARTS_WITH,
  MatchOption'
  #-}
