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
-- Module      : Amazonka.AccessAnalyzer.Types.PolicyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.PolicyType
  ( PolicyType
      ( ..,
        PolicyType_IDENTITY_POLICY,
        PolicyType_RESOURCE_POLICY,
        PolicyType_SERVICE_CONTROL_POLICY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PolicyType = PolicyType'
  { fromPolicyType ::
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

pattern PolicyType_IDENTITY_POLICY :: PolicyType
pattern PolicyType_IDENTITY_POLICY = PolicyType' "IDENTITY_POLICY"

pattern PolicyType_RESOURCE_POLICY :: PolicyType
pattern PolicyType_RESOURCE_POLICY = PolicyType' "RESOURCE_POLICY"

pattern PolicyType_SERVICE_CONTROL_POLICY :: PolicyType
pattern PolicyType_SERVICE_CONTROL_POLICY = PolicyType' "SERVICE_CONTROL_POLICY"

{-# COMPLETE
  PolicyType_IDENTITY_POLICY,
  PolicyType_RESOURCE_POLICY,
  PolicyType_SERVICE_CONTROL_POLICY,
  PolicyType'
  #-}
