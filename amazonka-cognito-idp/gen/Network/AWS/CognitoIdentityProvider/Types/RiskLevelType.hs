{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RiskLevelType
  ( RiskLevelType
      ( ..,
        RiskLevelType_High,
        RiskLevelType_Low,
        RiskLevelType_Medium
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RiskLevelType = RiskLevelType'
  { fromRiskLevelType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
