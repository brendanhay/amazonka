{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.PositionalConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.PositionalConstraint
  ( PositionalConstraint
    ( PositionalConstraint'
    , PositionalConstraintExactly
    , PositionalConstraintStartsWith
    , PositionalConstraintEndsWith
    , PositionalConstraintContains
    , PositionalConstraintContainsWord
    , fromPositionalConstraint
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PositionalConstraint = PositionalConstraint'{fromPositionalConstraint
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern PositionalConstraintExactly :: PositionalConstraint
pattern PositionalConstraintExactly = PositionalConstraint' "EXACTLY"

pattern PositionalConstraintStartsWith :: PositionalConstraint
pattern PositionalConstraintStartsWith = PositionalConstraint' "STARTS_WITH"

pattern PositionalConstraintEndsWith :: PositionalConstraint
pattern PositionalConstraintEndsWith = PositionalConstraint' "ENDS_WITH"

pattern PositionalConstraintContains :: PositionalConstraint
pattern PositionalConstraintContains = PositionalConstraint' "CONTAINS"

pattern PositionalConstraintContainsWord :: PositionalConstraint
pattern PositionalConstraintContainsWord = PositionalConstraint' "CONTAINS_WORD"

{-# COMPLETE 
  PositionalConstraintExactly,

  PositionalConstraintStartsWith,

  PositionalConstraintEndsWith,

  PositionalConstraintContains,

  PositionalConstraintContainsWord,
  PositionalConstraint'
  #-}
