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
-- Module      : Network.AWS.WAF.Types.PositionalConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.PositionalConstraint
  ( PositionalConstraint
      ( ..,
        PositionalConstraint_CONTAINS,
        PositionalConstraint_CONTAINS_WORD,
        PositionalConstraint_ENDS_WITH,
        PositionalConstraint_EXACTLY,
        PositionalConstraint_STARTS_WITH
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PositionalConstraint = PositionalConstraint'
  { fromPositionalConstraint ::
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

pattern PositionalConstraint_CONTAINS :: PositionalConstraint
pattern PositionalConstraint_CONTAINS = PositionalConstraint' "CONTAINS"

pattern PositionalConstraint_CONTAINS_WORD :: PositionalConstraint
pattern PositionalConstraint_CONTAINS_WORD = PositionalConstraint' "CONTAINS_WORD"

pattern PositionalConstraint_ENDS_WITH :: PositionalConstraint
pattern PositionalConstraint_ENDS_WITH = PositionalConstraint' "ENDS_WITH"

pattern PositionalConstraint_EXACTLY :: PositionalConstraint
pattern PositionalConstraint_EXACTLY = PositionalConstraint' "EXACTLY"

pattern PositionalConstraint_STARTS_WITH :: PositionalConstraint
pattern PositionalConstraint_STARTS_WITH = PositionalConstraint' "STARTS_WITH"

{-# COMPLETE
  PositionalConstraint_CONTAINS,
  PositionalConstraint_CONTAINS_WORD,
  PositionalConstraint_ENDS_WITH,
  PositionalConstraint_EXACTLY,
  PositionalConstraint_STARTS_WITH,
  PositionalConstraint'
  #-}
