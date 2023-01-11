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
-- Module      : Amazonka.Shield.Types.ProactiveEngagementStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProactiveEngagementStatus
  ( ProactiveEngagementStatus
      ( ..,
        ProactiveEngagementStatus_DISABLED,
        ProactiveEngagementStatus_ENABLED,
        ProactiveEngagementStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProactiveEngagementStatus = ProactiveEngagementStatus'
  { fromProactiveEngagementStatus ::
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
