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
-- Module      : Network.AWS.Shield.Types.ProactiveEngagementStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProactiveEngagementStatus
  ( ProactiveEngagementStatus
      ( ..,
        ProactiveEngagementStatus_DISABLED,
        ProactiveEngagementStatus_ENABLED,
        ProactiveEngagementStatus_PENDING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProactiveEngagementStatus = ProactiveEngagementStatus'
  { fromProactiveEngagementStatus ::
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

pattern ProactiveEngagementStatus_DISABLED :: ProactiveEngagementStatus
pattern ProactiveEngagementStatus_DISABLED = ProactiveEngagementStatus' "DISABLED"

pattern ProactiveEngagementStatus_ENABLED :: ProactiveEngagementStatus
pattern ProactiveEngagementStatus_ENABLED = ProactiveEngagementStatus' "ENABLED"

pattern ProactiveEngagementStatus_PENDING :: ProactiveEngagementStatus
pattern ProactiveEngagementStatus_PENDING = ProactiveEngagementStatus' "PENDING"

{-# COMPLETE
  ProactiveEngagementStatus_DISABLED,
  ProactiveEngagementStatus_ENABLED,
  ProactiveEngagementStatus_PENDING,
  ProactiveEngagementStatus'
  #-}
