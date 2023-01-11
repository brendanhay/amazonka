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
-- Module      : Amazonka.GroundStation.Types.ContactStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ContactStatus
  ( ContactStatus
      ( ..,
        ContactStatus_AVAILABLE,
        ContactStatus_AWS_CANCELLED,
        ContactStatus_AWS_FAILED,
        ContactStatus_CANCELLED,
        ContactStatus_CANCELLING,
        ContactStatus_COMPLETED,
        ContactStatus_FAILED,
        ContactStatus_FAILED_TO_SCHEDULE,
        ContactStatus_PASS,
        ContactStatus_POSTPASS,
        ContactStatus_PREPASS,
        ContactStatus_SCHEDULED,
        ContactStatus_SCHEDULING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContactStatus = ContactStatus'
  { fromContactStatus ::
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

pattern ContactStatus_AVAILABLE :: ContactStatus
pattern ContactStatus_AVAILABLE = ContactStatus' "AVAILABLE"

pattern ContactStatus_AWS_CANCELLED :: ContactStatus
pattern ContactStatus_AWS_CANCELLED = ContactStatus' "AWS_CANCELLED"

pattern ContactStatus_AWS_FAILED :: ContactStatus
pattern ContactStatus_AWS_FAILED = ContactStatus' "AWS_FAILED"

pattern ContactStatus_CANCELLED :: ContactStatus
pattern ContactStatus_CANCELLED = ContactStatus' "CANCELLED"

pattern ContactStatus_CANCELLING :: ContactStatus
pattern ContactStatus_CANCELLING = ContactStatus' "CANCELLING"

pattern ContactStatus_COMPLETED :: ContactStatus
pattern ContactStatus_COMPLETED = ContactStatus' "COMPLETED"

pattern ContactStatus_FAILED :: ContactStatus
pattern ContactStatus_FAILED = ContactStatus' "FAILED"

pattern ContactStatus_FAILED_TO_SCHEDULE :: ContactStatus
pattern ContactStatus_FAILED_TO_SCHEDULE = ContactStatus' "FAILED_TO_SCHEDULE"

pattern ContactStatus_PASS :: ContactStatus
pattern ContactStatus_PASS = ContactStatus' "PASS"

pattern ContactStatus_POSTPASS :: ContactStatus
pattern ContactStatus_POSTPASS = ContactStatus' "POSTPASS"

pattern ContactStatus_PREPASS :: ContactStatus
pattern ContactStatus_PREPASS = ContactStatus' "PREPASS"

pattern ContactStatus_SCHEDULED :: ContactStatus
pattern ContactStatus_SCHEDULED = ContactStatus' "SCHEDULED"

pattern ContactStatus_SCHEDULING :: ContactStatus
pattern ContactStatus_SCHEDULING = ContactStatus' "SCHEDULING"

{-# COMPLETE
  ContactStatus_AVAILABLE,
  ContactStatus_AWS_CANCELLED,
  ContactStatus_AWS_FAILED,
  ContactStatus_CANCELLED,
  ContactStatus_CANCELLING,
  ContactStatus_COMPLETED,
  ContactStatus_FAILED,
  ContactStatus_FAILED_TO_SCHEDULE,
  ContactStatus_PASS,
  ContactStatus_POSTPASS,
  ContactStatus_PREPASS,
  ContactStatus_SCHEDULED,
  ContactStatus_SCHEDULING,
  ContactStatus'
  #-}
