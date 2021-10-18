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
-- Module      : Network.AWS.CloudHSMv2.Types.HsmState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.HsmState
  ( HsmState
      ( ..,
        HsmState_ACTIVE,
        HsmState_CREATE_IN_PROGRESS,
        HsmState_DEGRADED,
        HsmState_DELETED,
        HsmState_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype HsmState = HsmState'
  { fromHsmState ::
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

pattern HsmState_ACTIVE :: HsmState
pattern HsmState_ACTIVE = HsmState' "ACTIVE"

pattern HsmState_CREATE_IN_PROGRESS :: HsmState
pattern HsmState_CREATE_IN_PROGRESS = HsmState' "CREATE_IN_PROGRESS"

pattern HsmState_DEGRADED :: HsmState
pattern HsmState_DEGRADED = HsmState' "DEGRADED"

pattern HsmState_DELETED :: HsmState
pattern HsmState_DELETED = HsmState' "DELETED"

pattern HsmState_DELETE_IN_PROGRESS :: HsmState
pattern HsmState_DELETE_IN_PROGRESS = HsmState' "DELETE_IN_PROGRESS"

{-# COMPLETE
  HsmState_ACTIVE,
  HsmState_CREATE_IN_PROGRESS,
  HsmState_DEGRADED,
  HsmState_DELETED,
  HsmState_DELETE_IN_PROGRESS,
  HsmState'
  #-}
