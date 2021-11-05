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
-- Module      : Amazonka.Detective.Types.MemberStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.MemberStatus
  ( MemberStatus
      ( ..,
        MemberStatus_ACCEPTED_BUT_DISABLED,
        MemberStatus_ENABLED,
        MemberStatus_INVITED,
        MemberStatus_VERIFICATION_FAILED,
        MemberStatus_VERIFICATION_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MemberStatus = MemberStatus'
  { fromMemberStatus ::
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

pattern MemberStatus_ACCEPTED_BUT_DISABLED :: MemberStatus
pattern MemberStatus_ACCEPTED_BUT_DISABLED = MemberStatus' "ACCEPTED_BUT_DISABLED"

pattern MemberStatus_ENABLED :: MemberStatus
pattern MemberStatus_ENABLED = MemberStatus' "ENABLED"

pattern MemberStatus_INVITED :: MemberStatus
pattern MemberStatus_INVITED = MemberStatus' "INVITED"

pattern MemberStatus_VERIFICATION_FAILED :: MemberStatus
pattern MemberStatus_VERIFICATION_FAILED = MemberStatus' "VERIFICATION_FAILED"

pattern MemberStatus_VERIFICATION_IN_PROGRESS :: MemberStatus
pattern MemberStatus_VERIFICATION_IN_PROGRESS = MemberStatus' "VERIFICATION_IN_PROGRESS"

{-# COMPLETE
  MemberStatus_ACCEPTED_BUT_DISABLED,
  MemberStatus_ENABLED,
  MemberStatus_INVITED,
  MemberStatus_VERIFICATION_FAILED,
  MemberStatus_VERIFICATION_IN_PROGRESS,
  MemberStatus'
  #-}
