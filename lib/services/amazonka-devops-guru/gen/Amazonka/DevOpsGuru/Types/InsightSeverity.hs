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
-- Module      : Amazonka.DevOpsGuru.Types.InsightSeverity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.InsightSeverity
  ( InsightSeverity
      ( ..,
        InsightSeverity_HIGH,
        InsightSeverity_LOW,
        InsightSeverity_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InsightSeverity = InsightSeverity'
  { fromInsightSeverity ::
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

pattern InsightSeverity_HIGH :: InsightSeverity
pattern InsightSeverity_HIGH = InsightSeverity' "HIGH"

pattern InsightSeverity_LOW :: InsightSeverity
pattern InsightSeverity_LOW = InsightSeverity' "LOW"

pattern InsightSeverity_MEDIUM :: InsightSeverity
pattern InsightSeverity_MEDIUM = InsightSeverity' "MEDIUM"

{-# COMPLETE
  InsightSeverity_HIGH,
  InsightSeverity_LOW,
  InsightSeverity_MEDIUM,
  InsightSeverity'
  #-}
