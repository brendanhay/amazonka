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
-- Module      : Amazonka.WAF.Types.PositionalConstraint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.PositionalConstraint
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PositionalConstraint = PositionalConstraint'
  { fromPositionalConstraint ::
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
