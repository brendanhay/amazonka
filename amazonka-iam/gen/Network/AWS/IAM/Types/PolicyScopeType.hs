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
-- Module      : Network.AWS.IAM.Types.PolicyScopeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyScopeType
  ( PolicyScopeType
      ( ..,
        PolicyScopeType_AWS,
        PolicyScopeType_All,
        PolicyScopeType_Local
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PolicyScopeType = PolicyScopeType'
  { fromPolicyScopeType ::
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

pattern PolicyScopeType_AWS :: PolicyScopeType
pattern PolicyScopeType_AWS = PolicyScopeType' "AWS"

pattern PolicyScopeType_All :: PolicyScopeType
pattern PolicyScopeType_All = PolicyScopeType' "All"

pattern PolicyScopeType_Local :: PolicyScopeType
pattern PolicyScopeType_Local = PolicyScopeType' "Local"

{-# COMPLETE
  PolicyScopeType_AWS,
  PolicyScopeType_All,
  PolicyScopeType_Local,
  PolicyScopeType'
  #-}
