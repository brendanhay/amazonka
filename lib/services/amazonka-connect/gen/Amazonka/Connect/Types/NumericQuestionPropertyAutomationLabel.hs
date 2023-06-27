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
-- Module      : Amazonka.Connect.Types.NumericQuestionPropertyAutomationLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.NumericQuestionPropertyAutomationLabel
  ( NumericQuestionPropertyAutomationLabel
      ( ..,
        NumericQuestionPropertyAutomationLabel_AGENT_INTERACTION_DURATION,
        NumericQuestionPropertyAutomationLabel_CONTACT_DURATION,
        NumericQuestionPropertyAutomationLabel_CUSTOMER_HOLD_TIME,
        NumericQuestionPropertyAutomationLabel_NON_TALK_TIME,
        NumericQuestionPropertyAutomationLabel_NON_TALK_TIME_PERCENTAGE,
        NumericQuestionPropertyAutomationLabel_NUMBER_OF_INTERRUPTIONS,
        NumericQuestionPropertyAutomationLabel_OVERALL_AGENT_SENTIMENT_SCORE,
        NumericQuestionPropertyAutomationLabel_OVERALL_CUSTOMER_SENTIMENT_SCORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NumericQuestionPropertyAutomationLabel = NumericQuestionPropertyAutomationLabel'
  { fromNumericQuestionPropertyAutomationLabel ::
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

pattern NumericQuestionPropertyAutomationLabel_AGENT_INTERACTION_DURATION :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_AGENT_INTERACTION_DURATION = NumericQuestionPropertyAutomationLabel' "AGENT_INTERACTION_DURATION"

pattern NumericQuestionPropertyAutomationLabel_CONTACT_DURATION :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_CONTACT_DURATION = NumericQuestionPropertyAutomationLabel' "CONTACT_DURATION"

pattern NumericQuestionPropertyAutomationLabel_CUSTOMER_HOLD_TIME :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_CUSTOMER_HOLD_TIME = NumericQuestionPropertyAutomationLabel' "CUSTOMER_HOLD_TIME"

pattern NumericQuestionPropertyAutomationLabel_NON_TALK_TIME :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_NON_TALK_TIME = NumericQuestionPropertyAutomationLabel' "NON_TALK_TIME"

pattern NumericQuestionPropertyAutomationLabel_NON_TALK_TIME_PERCENTAGE :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_NON_TALK_TIME_PERCENTAGE = NumericQuestionPropertyAutomationLabel' "NON_TALK_TIME_PERCENTAGE"

pattern NumericQuestionPropertyAutomationLabel_NUMBER_OF_INTERRUPTIONS :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_NUMBER_OF_INTERRUPTIONS = NumericQuestionPropertyAutomationLabel' "NUMBER_OF_INTERRUPTIONS"

pattern NumericQuestionPropertyAutomationLabel_OVERALL_AGENT_SENTIMENT_SCORE :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_OVERALL_AGENT_SENTIMENT_SCORE = NumericQuestionPropertyAutomationLabel' "OVERALL_AGENT_SENTIMENT_SCORE"

pattern NumericQuestionPropertyAutomationLabel_OVERALL_CUSTOMER_SENTIMENT_SCORE :: NumericQuestionPropertyAutomationLabel
pattern NumericQuestionPropertyAutomationLabel_OVERALL_CUSTOMER_SENTIMENT_SCORE = NumericQuestionPropertyAutomationLabel' "OVERALL_CUSTOMER_SENTIMENT_SCORE"

{-# COMPLETE
  NumericQuestionPropertyAutomationLabel_AGENT_INTERACTION_DURATION,
  NumericQuestionPropertyAutomationLabel_CONTACT_DURATION,
  NumericQuestionPropertyAutomationLabel_CUSTOMER_HOLD_TIME,
  NumericQuestionPropertyAutomationLabel_NON_TALK_TIME,
  NumericQuestionPropertyAutomationLabel_NON_TALK_TIME_PERCENTAGE,
  NumericQuestionPropertyAutomationLabel_NUMBER_OF_INTERRUPTIONS,
  NumericQuestionPropertyAutomationLabel_OVERALL_AGENT_SENTIMENT_SCORE,
  NumericQuestionPropertyAutomationLabel_OVERALL_CUSTOMER_SENTIMENT_SCORE,
  NumericQuestionPropertyAutomationLabel'
  #-}
