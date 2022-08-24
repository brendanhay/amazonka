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
-- Module      : Amazonka.Connect.Types.PhoneNumberWorkflowStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PhoneNumberWorkflowStatus
  ( PhoneNumberWorkflowStatus
      ( ..,
        PhoneNumberWorkflowStatus_CLAIMED,
        PhoneNumberWorkflowStatus_FAILED,
        PhoneNumberWorkflowStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PhoneNumberWorkflowStatus = PhoneNumberWorkflowStatus'
  { fromPhoneNumberWorkflowStatus ::
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

pattern PhoneNumberWorkflowStatus_CLAIMED :: PhoneNumberWorkflowStatus
pattern PhoneNumberWorkflowStatus_CLAIMED = PhoneNumberWorkflowStatus' "CLAIMED"

pattern PhoneNumberWorkflowStatus_FAILED :: PhoneNumberWorkflowStatus
pattern PhoneNumberWorkflowStatus_FAILED = PhoneNumberWorkflowStatus' "FAILED"

pattern PhoneNumberWorkflowStatus_IN_PROGRESS :: PhoneNumberWorkflowStatus
pattern PhoneNumberWorkflowStatus_IN_PROGRESS = PhoneNumberWorkflowStatus' "IN_PROGRESS"

{-# COMPLETE
  PhoneNumberWorkflowStatus_CLAIMED,
  PhoneNumberWorkflowStatus_FAILED,
  PhoneNumberWorkflowStatus_IN_PROGRESS,
  PhoneNumberWorkflowStatus'
  #-}
