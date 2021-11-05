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
-- Module      : Network.AWS.WellArchitected.Types.ChoiceReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.ChoiceReason
  ( ChoiceReason
      ( ..,
        ChoiceReason_ARCHITECTURE_CONSTRAINTS,
        ChoiceReason_BUSINESS_PRIORITIES,
        ChoiceReason_NONE,
        ChoiceReason_OTHER,
        ChoiceReason_OUT_OF_SCOPE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ChoiceReason = ChoiceReason'
  { fromChoiceReason ::
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

pattern ChoiceReason_ARCHITECTURE_CONSTRAINTS :: ChoiceReason
pattern ChoiceReason_ARCHITECTURE_CONSTRAINTS = ChoiceReason' "ARCHITECTURE_CONSTRAINTS"

pattern ChoiceReason_BUSINESS_PRIORITIES :: ChoiceReason
pattern ChoiceReason_BUSINESS_PRIORITIES = ChoiceReason' "BUSINESS_PRIORITIES"

pattern ChoiceReason_NONE :: ChoiceReason
pattern ChoiceReason_NONE = ChoiceReason' "NONE"

pattern ChoiceReason_OTHER :: ChoiceReason
pattern ChoiceReason_OTHER = ChoiceReason' "OTHER"

pattern ChoiceReason_OUT_OF_SCOPE :: ChoiceReason
pattern ChoiceReason_OUT_OF_SCOPE = ChoiceReason' "OUT_OF_SCOPE"

{-# COMPLETE
  ChoiceReason_ARCHITECTURE_CONSTRAINTS,
  ChoiceReason_BUSINESS_PRIORITIES,
  ChoiceReason_NONE,
  ChoiceReason_OTHER,
  ChoiceReason_OUT_OF_SCOPE,
  ChoiceReason'
  #-}
