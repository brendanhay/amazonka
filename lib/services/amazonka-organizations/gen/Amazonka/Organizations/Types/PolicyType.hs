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
-- Module      : Amazonka.Organizations.Types.PolicyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.PolicyType
  ( PolicyType
      ( ..,
        PolicyType_AISERVICES_OPT_OUT_POLICY,
        PolicyType_BACKUP_POLICY,
        PolicyType_SERVICE_CONTROL_POLICY,
        PolicyType_TAG_POLICY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PolicyType = PolicyType'
  { fromPolicyType ::
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
