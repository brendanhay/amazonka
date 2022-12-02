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
-- Module      : Amazonka.Backup.Types.RecoveryPointStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RecoveryPointStatus
  ( RecoveryPointStatus
      ( ..,
        RecoveryPointStatus_COMPLETED,
        RecoveryPointStatus_DELETING,
        RecoveryPointStatus_EXPIRED,
        RecoveryPointStatus_PARTIAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecoveryPointStatus = RecoveryPointStatus'
  { fromRecoveryPointStatus ::
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

pattern RecoveryPointStatus_COMPLETED :: RecoveryPointStatus
pattern RecoveryPointStatus_COMPLETED = RecoveryPointStatus' "COMPLETED"

pattern RecoveryPointStatus_DELETING :: RecoveryPointStatus
pattern RecoveryPointStatus_DELETING = RecoveryPointStatus' "DELETING"

pattern RecoveryPointStatus_EXPIRED :: RecoveryPointStatus
pattern RecoveryPointStatus_EXPIRED = RecoveryPointStatus' "EXPIRED"

pattern RecoveryPointStatus_PARTIAL :: RecoveryPointStatus
pattern RecoveryPointStatus_PARTIAL = RecoveryPointStatus' "PARTIAL"

{-# COMPLETE
  RecoveryPointStatus_COMPLETED,
  RecoveryPointStatus_DELETING,
  RecoveryPointStatus_EXPIRED,
  RecoveryPointStatus_PARTIAL,
  RecoveryPointStatus'
  #-}
