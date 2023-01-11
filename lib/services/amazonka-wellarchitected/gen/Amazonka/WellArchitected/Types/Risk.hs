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
-- Module      : Amazonka.WellArchitected.Types.Risk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Risk
  ( Risk
      ( ..,
        Risk_HIGH,
        Risk_MEDIUM,
        Risk_NONE,
        Risk_NOT_APPLICABLE,
        Risk_UNANSWERED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The risk for a given workload, lens review, pillar, or question.
newtype Risk = Risk' {fromRisk :: Data.Text}
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

pattern Risk_HIGH :: Risk
pattern Risk_HIGH = Risk' "HIGH"

pattern Risk_MEDIUM :: Risk
pattern Risk_MEDIUM = Risk' "MEDIUM"

pattern Risk_NONE :: Risk
pattern Risk_NONE = Risk' "NONE"

pattern Risk_NOT_APPLICABLE :: Risk
pattern Risk_NOT_APPLICABLE = Risk' "NOT_APPLICABLE"

pattern Risk_UNANSWERED :: Risk
pattern Risk_UNANSWERED = Risk' "UNANSWERED"

{-# COMPLETE
  Risk_HIGH,
  Risk_MEDIUM,
  Risk_NONE,
  Risk_NOT_APPLICABLE,
  Risk_UNANSWERED,
  Risk'
  #-}
