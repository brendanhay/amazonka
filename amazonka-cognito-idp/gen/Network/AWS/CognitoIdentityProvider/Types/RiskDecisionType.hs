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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskDecisionType
  ( RiskDecisionType
      ( ..,
        RiskDecisionType_AccountTakeover,
        RiskDecisionType_Block,
        RiskDecisionType_NoRisk
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RiskDecisionType = RiskDecisionType'
  { fromRiskDecisionType ::
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

pattern RiskDecisionType_AccountTakeover :: RiskDecisionType
pattern RiskDecisionType_AccountTakeover = RiskDecisionType' "AccountTakeover"

pattern RiskDecisionType_Block :: RiskDecisionType
pattern RiskDecisionType_Block = RiskDecisionType' "Block"

pattern RiskDecisionType_NoRisk :: RiskDecisionType
pattern RiskDecisionType_NoRisk = RiskDecisionType' "NoRisk"

{-# COMPLETE
  RiskDecisionType_AccountTakeover,
  RiskDecisionType_Block,
  RiskDecisionType_NoRisk,
  RiskDecisionType'
  #-}
