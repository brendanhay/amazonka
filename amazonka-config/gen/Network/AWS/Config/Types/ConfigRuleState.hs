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
-- Module      : Network.AWS.Config.Types.ConfigRuleState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleState
  ( ConfigRuleState
      ( ..,
        ConfigRuleState_ACTIVE,
        ConfigRuleState_DELETING,
        ConfigRuleState_DELETING_RESULTS,
        ConfigRuleState_EVALUATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConfigRuleState = ConfigRuleState'
  { fromConfigRuleState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ConfigRuleState_ACTIVE :: ConfigRuleState
pattern ConfigRuleState_ACTIVE = ConfigRuleState' "ACTIVE"

pattern ConfigRuleState_DELETING :: ConfigRuleState
pattern ConfigRuleState_DELETING = ConfigRuleState' "DELETING"

pattern ConfigRuleState_DELETING_RESULTS :: ConfigRuleState
pattern ConfigRuleState_DELETING_RESULTS = ConfigRuleState' "DELETING_RESULTS"

pattern ConfigRuleState_EVALUATING :: ConfigRuleState
pattern ConfigRuleState_EVALUATING = ConfigRuleState' "EVALUATING"

{-# COMPLETE
  ConfigRuleState_ACTIVE,
  ConfigRuleState_DELETING,
  ConfigRuleState_DELETING_RESULTS,
  ConfigRuleState_EVALUATING,
  ConfigRuleState'
  #-}
