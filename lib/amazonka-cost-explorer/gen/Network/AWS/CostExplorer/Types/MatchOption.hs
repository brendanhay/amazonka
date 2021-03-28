{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MatchOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.MatchOption
  ( MatchOption
    ( MatchOption'
    , MatchOptionEquals
    , MatchOptionStartsWith
    , MatchOptionEndsWith
    , MatchOptionContains
    , MatchOptionCaseSensitive
    , MatchOptionCaseInsensitive
    , fromMatchOption
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MatchOption = MatchOption'{fromMatchOption :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern MatchOptionEquals :: MatchOption
pattern MatchOptionEquals = MatchOption' "EQUALS"

pattern MatchOptionStartsWith :: MatchOption
pattern MatchOptionStartsWith = MatchOption' "STARTS_WITH"

pattern MatchOptionEndsWith :: MatchOption
pattern MatchOptionEndsWith = MatchOption' "ENDS_WITH"

pattern MatchOptionContains :: MatchOption
pattern MatchOptionContains = MatchOption' "CONTAINS"

pattern MatchOptionCaseSensitive :: MatchOption
pattern MatchOptionCaseSensitive = MatchOption' "CASE_SENSITIVE"

pattern MatchOptionCaseInsensitive :: MatchOption
pattern MatchOptionCaseInsensitive = MatchOption' "CASE_INSENSITIVE"

{-# COMPLETE 
  MatchOptionEquals,

  MatchOptionStartsWith,

  MatchOptionEndsWith,

  MatchOptionContains,

  MatchOptionCaseSensitive,

  MatchOptionCaseInsensitive,
  MatchOption'
  #-}
