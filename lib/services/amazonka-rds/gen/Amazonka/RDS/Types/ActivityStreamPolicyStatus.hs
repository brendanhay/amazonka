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
-- Module      : Amazonka.RDS.Types.ActivityStreamPolicyStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ActivityStreamPolicyStatus
  ( ActivityStreamPolicyStatus
      ( ..,
        ActivityStreamPolicyStatus_Locked,
        ActivityStreamPolicyStatus_Locking_policy,
        ActivityStreamPolicyStatus_Unlocked,
        ActivityStreamPolicyStatus_Unlocking_policy
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ActivityStreamPolicyStatus = ActivityStreamPolicyStatus'
  { fromActivityStreamPolicyStatus ::
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

pattern ActivityStreamPolicyStatus_Locked :: ActivityStreamPolicyStatus
pattern ActivityStreamPolicyStatus_Locked = ActivityStreamPolicyStatus' "locked"

pattern ActivityStreamPolicyStatus_Locking_policy :: ActivityStreamPolicyStatus
pattern ActivityStreamPolicyStatus_Locking_policy = ActivityStreamPolicyStatus' "locking-policy"

pattern ActivityStreamPolicyStatus_Unlocked :: ActivityStreamPolicyStatus
pattern ActivityStreamPolicyStatus_Unlocked = ActivityStreamPolicyStatus' "unlocked"

pattern ActivityStreamPolicyStatus_Unlocking_policy :: ActivityStreamPolicyStatus
pattern ActivityStreamPolicyStatus_Unlocking_policy = ActivityStreamPolicyStatus' "unlocking-policy"

{-# COMPLETE
  ActivityStreamPolicyStatus_Locked,
  ActivityStreamPolicyStatus_Locking_policy,
  ActivityStreamPolicyStatus_Unlocked,
  ActivityStreamPolicyStatus_Unlocking_policy,
  ActivityStreamPolicyStatus'
  #-}
