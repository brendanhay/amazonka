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
-- Module      : Network.AWS.IoT.Types.AuditFindingSeverity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditFindingSeverity
  ( AuditFindingSeverity
      ( ..,
        AuditFindingSeverity_CRITICAL,
        AuditFindingSeverity_HIGH,
        AuditFindingSeverity_LOW,
        AuditFindingSeverity_MEDIUM
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuditFindingSeverity = AuditFindingSeverity'
  { fromAuditFindingSeverity ::
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
