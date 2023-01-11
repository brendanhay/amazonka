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
-- Module      : Amazonka.Chime.Types.PhoneNumberStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberStatus
  ( PhoneNumberStatus
      ( ..,
        PhoneNumberStatus_AcquireFailed,
        PhoneNumberStatus_AcquireInProgress,
        PhoneNumberStatus_Assigned,
        PhoneNumberStatus_DeleteFailed,
        PhoneNumberStatus_DeleteInProgress,
        PhoneNumberStatus_ReleaseFailed,
        PhoneNumberStatus_ReleaseInProgress,
        PhoneNumberStatus_Unassigned
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberStatus = PhoneNumberStatus'
  { fromPhoneNumberStatus ::
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

pattern PhoneNumberStatus_AcquireFailed :: PhoneNumberStatus
pattern PhoneNumberStatus_AcquireFailed = PhoneNumberStatus' "AcquireFailed"

pattern PhoneNumberStatus_AcquireInProgress :: PhoneNumberStatus
pattern PhoneNumberStatus_AcquireInProgress = PhoneNumberStatus' "AcquireInProgress"

pattern PhoneNumberStatus_Assigned :: PhoneNumberStatus
pattern PhoneNumberStatus_Assigned = PhoneNumberStatus' "Assigned"

pattern PhoneNumberStatus_DeleteFailed :: PhoneNumberStatus
pattern PhoneNumberStatus_DeleteFailed = PhoneNumberStatus' "DeleteFailed"

pattern PhoneNumberStatus_DeleteInProgress :: PhoneNumberStatus
pattern PhoneNumberStatus_DeleteInProgress = PhoneNumberStatus' "DeleteInProgress"

pattern PhoneNumberStatus_ReleaseFailed :: PhoneNumberStatus
pattern PhoneNumberStatus_ReleaseFailed = PhoneNumberStatus' "ReleaseFailed"

pattern PhoneNumberStatus_ReleaseInProgress :: PhoneNumberStatus
pattern PhoneNumberStatus_ReleaseInProgress = PhoneNumberStatus' "ReleaseInProgress"

pattern PhoneNumberStatus_Unassigned :: PhoneNumberStatus
pattern PhoneNumberStatus_Unassigned = PhoneNumberStatus' "Unassigned"

{-# COMPLETE
  PhoneNumberStatus_AcquireFailed,
  PhoneNumberStatus_AcquireInProgress,
  PhoneNumberStatus_Assigned,
  PhoneNumberStatus_DeleteFailed,
  PhoneNumberStatus_DeleteInProgress,
  PhoneNumberStatus_ReleaseFailed,
  PhoneNumberStatus_ReleaseInProgress,
  PhoneNumberStatus_Unassigned,
  PhoneNumberStatus'
  #-}
