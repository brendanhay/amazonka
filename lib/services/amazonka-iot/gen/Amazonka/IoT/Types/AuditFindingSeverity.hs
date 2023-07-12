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
-- Module      : Amazonka.IoT.Types.AuditFindingSeverity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditFindingSeverity
  ( AuditFindingSeverity
      ( ..,
        AuditFindingSeverity_CRITICAL,
        AuditFindingSeverity_HIGH,
        AuditFindingSeverity_LOW,
        AuditFindingSeverity_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuditFindingSeverity = AuditFindingSeverity'
  { fromAuditFindingSeverity ::
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

pattern AuditFindingSeverity_CRITICAL :: AuditFindingSeverity
pattern AuditFindingSeverity_CRITICAL = AuditFindingSeverity' "CRITICAL"

pattern AuditFindingSeverity_HIGH :: AuditFindingSeverity
pattern AuditFindingSeverity_HIGH = AuditFindingSeverity' "HIGH"

pattern AuditFindingSeverity_LOW :: AuditFindingSeverity
pattern AuditFindingSeverity_LOW = AuditFindingSeverity' "LOW"

pattern AuditFindingSeverity_MEDIUM :: AuditFindingSeverity
pattern AuditFindingSeverity_MEDIUM = AuditFindingSeverity' "MEDIUM"

{-# COMPLETE
  AuditFindingSeverity_CRITICAL,
  AuditFindingSeverity_HIGH,
  AuditFindingSeverity_LOW,
  AuditFindingSeverity_MEDIUM,
  AuditFindingSeverity'
  #-}
