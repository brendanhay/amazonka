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
-- Module      : Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRuleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRuleType
  ( ConfiguredTableAnalysisRuleType
      ( ..,
        ConfiguredTableAnalysisRuleType_AGGREGATION,
        ConfiguredTableAnalysisRuleType_LIST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfiguredTableAnalysisRuleType = ConfiguredTableAnalysisRuleType'
  { fromConfiguredTableAnalysisRuleType ::
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

pattern ConfiguredTableAnalysisRuleType_AGGREGATION :: ConfiguredTableAnalysisRuleType
pattern ConfiguredTableAnalysisRuleType_AGGREGATION = ConfiguredTableAnalysisRuleType' "AGGREGATION"

pattern ConfiguredTableAnalysisRuleType_LIST :: ConfiguredTableAnalysisRuleType
pattern ConfiguredTableAnalysisRuleType_LIST = ConfiguredTableAnalysisRuleType' "LIST"

{-# COMPLETE
  ConfiguredTableAnalysisRuleType_AGGREGATION,
  ConfiguredTableAnalysisRuleType_LIST,
  ConfiguredTableAnalysisRuleType'
  #-}
