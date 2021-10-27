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
-- Module      : Network.AWS.SSMIncidents.Types.SsmTargetAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.SsmTargetAccount
  ( SsmTargetAccount
      ( ..,
        SsmTargetAccount_IMPACTED_ACCOUNT,
        SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SsmTargetAccount = SsmTargetAccount'
  { fromSsmTargetAccount ::
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

pattern SsmTargetAccount_IMPACTED_ACCOUNT :: SsmTargetAccount
pattern SsmTargetAccount_IMPACTED_ACCOUNT = SsmTargetAccount' "IMPACTED_ACCOUNT"

pattern SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT :: SsmTargetAccount
pattern SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT = SsmTargetAccount' "RESPONSE_PLAN_OWNER_ACCOUNT"

{-# COMPLETE
  SsmTargetAccount_IMPACTED_ACCOUNT,
  SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT,
  SsmTargetAccount'
  #-}
