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
-- Module      : Amazonka.IoT.Types.AuditFrequency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditFrequency
  ( AuditFrequency
      ( ..,
        AuditFrequency_BIWEEKLY,
        AuditFrequency_DAILY,
        AuditFrequency_MONTHLY,
        AuditFrequency_WEEKLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuditFrequency = AuditFrequency'
  { fromAuditFrequency ::
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
