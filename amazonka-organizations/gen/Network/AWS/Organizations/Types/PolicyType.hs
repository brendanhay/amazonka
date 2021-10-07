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
-- Module      : Network.AWS.Organizations.Types.PolicyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyType
  ( PolicyType
      ( ..,
        PolicyType_AISERVICES_OPT_OUT_POLICY,
        PolicyType_BACKUP_POLICY,
        PolicyType_SERVICE_CONTROL_POLICY,
        PolicyType_TAG_POLICY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

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

pattern PolicyType_AISERVICES_OPT_OUT_POLICY :: PolicyType
pattern PolicyType_AISERVICES_OPT_OUT_POLICY = PolicyType' "AISERVICES_OPT_OUT_POLICY"

pattern PolicyType_BACKUP_POLICY :: PolicyType
pattern PolicyType_BACKUP_POLICY = PolicyType' "BACKUP_POLICY"

pattern PolicyType_SERVICE_CONTROL_POLICY :: PolicyType
pattern PolicyType_SERVICE_CONTROL_POLICY = PolicyType' "SERVICE_CONTROL_POLICY"

pattern PolicyType_TAG_POLICY :: PolicyType
pattern PolicyType_TAG_POLICY = PolicyType' "TAG_POLICY"

{-# COMPLETE
  PolicyType_AISERVICES_OPT_OUT_POLICY,
  PolicyType_BACKUP_POLICY,
  PolicyType_SERVICE_CONTROL_POLICY,
  PolicyType_TAG_POLICY,
  PolicyType'
  #-}
