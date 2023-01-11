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
-- Module      : Amazonka.CloudHSMV2.Types.HsmState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSMV2.Types.HsmState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HsmState = HsmState'
  { fromHsmState ::
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
