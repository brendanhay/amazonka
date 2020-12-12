{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.PositionalConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.PositionalConstraint
  ( PositionalConstraint
      ( PositionalConstraint',
        Contains,
        ContainsWord,
        EndsWith,
        Exactly,
        StartsWith
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PositionalConstraint = PositionalConstraint' Lude.Text
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

pattern Contains :: PositionalConstraint
pattern Contains = PositionalConstraint' "CONTAINS"

pattern ContainsWord :: PositionalConstraint
pattern ContainsWord = PositionalConstraint' "CONTAINS_WORD"

pattern EndsWith :: PositionalConstraint
pattern EndsWith = PositionalConstraint' "ENDS_WITH"

pattern Exactly :: PositionalConstraint
pattern Exactly = PositionalConstraint' "EXACTLY"

pattern StartsWith :: PositionalConstraint
pattern StartsWith = PositionalConstraint' "STARTS_WITH"

{-# COMPLETE
  Contains,
  ContainsWord,
  EndsWith,
  Exactly,
  StartsWith,
  PositionalConstraint'
  #-}
