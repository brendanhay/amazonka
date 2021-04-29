{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype AuditFrequency = AuditFrequency'
  { fromAuditFrequency ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
