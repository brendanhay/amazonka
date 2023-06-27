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
-- Module      : Amazonka.Connect.Types.SingleSelectQuestionRuleCategoryAutomationCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.SingleSelectQuestionRuleCategoryAutomationCondition
  ( SingleSelectQuestionRuleCategoryAutomationCondition
      ( ..,
        SingleSelectQuestionRuleCategoryAutomationCondition_NOT_PRESENT,
        SingleSelectQuestionRuleCategoryAutomationCondition_PRESENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SingleSelectQuestionRuleCategoryAutomationCondition = SingleSelectQuestionRuleCategoryAutomationCondition'
  { fromSingleSelectQuestionRuleCategoryAutomationCondition ::
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

pattern SingleSelectQuestionRuleCategoryAutomationCondition_NOT_PRESENT :: SingleSelectQuestionRuleCategoryAutomationCondition
pattern SingleSelectQuestionRuleCategoryAutomationCondition_NOT_PRESENT = SingleSelectQuestionRuleCategoryAutomationCondition' "NOT_PRESENT"

pattern SingleSelectQuestionRuleCategoryAutomationCondition_PRESENT :: SingleSelectQuestionRuleCategoryAutomationCondition
pattern SingleSelectQuestionRuleCategoryAutomationCondition_PRESENT = SingleSelectQuestionRuleCategoryAutomationCondition' "PRESENT"

{-# COMPLETE
  SingleSelectQuestionRuleCategoryAutomationCondition_NOT_PRESENT,
  SingleSelectQuestionRuleCategoryAutomationCondition_PRESENT,
  SingleSelectQuestionRuleCategoryAutomationCondition'
  #-}
