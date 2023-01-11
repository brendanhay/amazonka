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
-- Module      : Amazonka.WellArchitected.Types.ChoiceReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ChoiceReason
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChoiceReason = ChoiceReason'
  { fromChoiceReason ::
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
