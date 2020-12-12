{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.MatchOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.MatchOption
  ( MatchOption
      ( MatchOption',
        CaseInsensitive,
        CaseSensitive,
        Contains,
        EndsWith,
        Equals,
        StartsWith
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MatchOption = MatchOption' Lude.Text
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

pattern CaseInsensitive :: MatchOption
pattern CaseInsensitive = MatchOption' "CASE_INSENSITIVE"

pattern CaseSensitive :: MatchOption
pattern CaseSensitive = MatchOption' "CASE_SENSITIVE"

pattern Contains :: MatchOption
pattern Contains = MatchOption' "CONTAINS"

pattern EndsWith :: MatchOption
pattern EndsWith = MatchOption' "ENDS_WITH"

pattern Equals :: MatchOption
pattern Equals = MatchOption' "EQUALS"

pattern StartsWith :: MatchOption
pattern StartsWith = MatchOption' "STARTS_WITH"

{-# COMPLETE
  CaseInsensitive,
  CaseSensitive,
  Contains,
  EndsWith,
  Equals,
  StartsWith,
  MatchOption'
  #-}
