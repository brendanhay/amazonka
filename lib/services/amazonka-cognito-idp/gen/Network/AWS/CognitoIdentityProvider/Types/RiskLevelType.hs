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
-- Module      : Amazonka.CognitoIdentityProvider.Types.RiskLevelType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.RiskLevelType
  ( RiskLevelType
      ( ..,
        RiskLevelType_High,
        RiskLevelType_Low,
        RiskLevelType_Medium
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RiskLevelType = RiskLevelType'
  { fromRiskLevelType ::
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

pattern RiskLevelType_High :: RiskLevelType
pattern RiskLevelType_High = RiskLevelType' "High"

pattern RiskLevelType_Low :: RiskLevelType
pattern RiskLevelType_Low = RiskLevelType' "Low"

pattern RiskLevelType_Medium :: RiskLevelType
pattern RiskLevelType_Medium = RiskLevelType' "Medium"

{-# COMPLETE
  RiskLevelType_High,
  RiskLevelType_Low,
  RiskLevelType_Medium,
  RiskLevelType'
  #-}
