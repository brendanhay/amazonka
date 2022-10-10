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
-- Module      : Amazonka.CodeStarNotifications.Types.TargetStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.TargetStatus
  ( TargetStatus
      ( ..,
        TargetStatus_ACTIVE,
        TargetStatus_DEACTIVATED,
        TargetStatus_INACTIVE,
        TargetStatus_PENDING,
        TargetStatus_UNREACHABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TargetStatus = TargetStatus'
  { fromTargetStatus ::
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

pattern TargetStatus_ACTIVE :: TargetStatus
pattern TargetStatus_ACTIVE = TargetStatus' "ACTIVE"

pattern TargetStatus_DEACTIVATED :: TargetStatus
pattern TargetStatus_DEACTIVATED = TargetStatus' "DEACTIVATED"

pattern TargetStatus_INACTIVE :: TargetStatus
pattern TargetStatus_INACTIVE = TargetStatus' "INACTIVE"

pattern TargetStatus_PENDING :: TargetStatus
pattern TargetStatus_PENDING = TargetStatus' "PENDING"

pattern TargetStatus_UNREACHABLE :: TargetStatus
pattern TargetStatus_UNREACHABLE = TargetStatus' "UNREACHABLE"

{-# COMPLETE
  TargetStatus_ACTIVE,
  TargetStatus_DEACTIVATED,
  TargetStatus_INACTIVE,
  TargetStatus_PENDING,
  TargetStatus_UNREACHABLE,
  TargetStatus'
  #-}
