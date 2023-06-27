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
-- Module      : Amazonka.LexV2Models.Types.TestResultTypeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TestResultTypeFilter
  ( TestResultTypeFilter
      ( ..,
        TestResultTypeFilter_ConversationLevelTestResults,
        TestResultTypeFilter_IntentClassificationTestResults,
        TestResultTypeFilter_OverallTestResults,
        TestResultTypeFilter_SlotResolutionTestResults,
        TestResultTypeFilter_UtteranceLevelResults
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestResultTypeFilter = TestResultTypeFilter'
  { fromTestResultTypeFilter ::
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

pattern TestResultTypeFilter_ConversationLevelTestResults :: TestResultTypeFilter
pattern TestResultTypeFilter_ConversationLevelTestResults = TestResultTypeFilter' "ConversationLevelTestResults"

pattern TestResultTypeFilter_IntentClassificationTestResults :: TestResultTypeFilter
pattern TestResultTypeFilter_IntentClassificationTestResults = TestResultTypeFilter' "IntentClassificationTestResults"

pattern TestResultTypeFilter_OverallTestResults :: TestResultTypeFilter
pattern TestResultTypeFilter_OverallTestResults = TestResultTypeFilter' "OverallTestResults"

pattern TestResultTypeFilter_SlotResolutionTestResults :: TestResultTypeFilter
pattern TestResultTypeFilter_SlotResolutionTestResults = TestResultTypeFilter' "SlotResolutionTestResults"

pattern TestResultTypeFilter_UtteranceLevelResults :: TestResultTypeFilter
pattern TestResultTypeFilter_UtteranceLevelResults = TestResultTypeFilter' "UtteranceLevelResults"

{-# COMPLETE
  TestResultTypeFilter_ConversationLevelTestResults,
  TestResultTypeFilter_IntentClassificationTestResults,
  TestResultTypeFilter_OverallTestResults,
  TestResultTypeFilter_SlotResolutionTestResults,
  TestResultTypeFilter_UtteranceLevelResults,
  TestResultTypeFilter'
  #-}
