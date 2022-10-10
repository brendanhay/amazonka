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
-- Module      : Amazonka.WellArchitected.Types.AnswerReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.AnswerReason
  ( AnswerReason
      ( ..,
        AnswerReason_ARCHITECTURE_CONSTRAINTS,
        AnswerReason_BUSINESS_PRIORITIES,
        AnswerReason_NONE,
        AnswerReason_OTHER,
        AnswerReason_OUT_OF_SCOPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AnswerReason = AnswerReason'
  { fromAnswerReason ::
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

pattern AnswerReason_ARCHITECTURE_CONSTRAINTS :: AnswerReason
pattern AnswerReason_ARCHITECTURE_CONSTRAINTS = AnswerReason' "ARCHITECTURE_CONSTRAINTS"

pattern AnswerReason_BUSINESS_PRIORITIES :: AnswerReason
pattern AnswerReason_BUSINESS_PRIORITIES = AnswerReason' "BUSINESS_PRIORITIES"

pattern AnswerReason_NONE :: AnswerReason
pattern AnswerReason_NONE = AnswerReason' "NONE"

pattern AnswerReason_OTHER :: AnswerReason
pattern AnswerReason_OTHER = AnswerReason' "OTHER"

pattern AnswerReason_OUT_OF_SCOPE :: AnswerReason
pattern AnswerReason_OUT_OF_SCOPE = AnswerReason' "OUT_OF_SCOPE"

{-# COMPLETE
  AnswerReason_ARCHITECTURE_CONSTRAINTS,
  AnswerReason_BUSINESS_PRIORITIES,
  AnswerReason_NONE,
  AnswerReason_OTHER,
  AnswerReason_OUT_OF_SCOPE,
  AnswerReason'
  #-}
