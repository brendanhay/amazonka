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
-- Module      : Network.AWS.IoT.Types.AuditFrequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFrequency
  ( AuditFrequency
      ( ..,
        AuditFrequency_BIWEEKLY,
        AuditFrequency_DAILY,
        AuditFrequency_MONTHLY,
        AuditFrequency_WEEKLY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AuditFrequency = AuditFrequency'
  { fromAuditFrequency ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern AuditFrequency_BIWEEKLY :: AuditFrequency
pattern AuditFrequency_BIWEEKLY = AuditFrequency' "BIWEEKLY"

pattern AuditFrequency_DAILY :: AuditFrequency
pattern AuditFrequency_DAILY = AuditFrequency' "DAILY"

pattern AuditFrequency_MONTHLY :: AuditFrequency
pattern AuditFrequency_MONTHLY = AuditFrequency' "MONTHLY"

pattern AuditFrequency_WEEKLY :: AuditFrequency
pattern AuditFrequency_WEEKLY = AuditFrequency' "WEEKLY"

{-# COMPLETE
  AuditFrequency_BIWEEKLY,
  AuditFrequency_DAILY,
  AuditFrequency_MONTHLY,
  AuditFrequency_WEEKLY,
  AuditFrequency'
  #-}
