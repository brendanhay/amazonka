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
-- Module      : Amazonka.Backup.Types.LegalHoldStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.LegalHoldStatus
  ( LegalHoldStatus
      ( ..,
        LegalHoldStatus_ACTIVE,
        LegalHoldStatus_CANCELED,
        LegalHoldStatus_CANCELING,
        LegalHoldStatus_CREATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LegalHoldStatus = LegalHoldStatus'
  { fromLegalHoldStatus ::
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

pattern LegalHoldStatus_ACTIVE :: LegalHoldStatus
pattern LegalHoldStatus_ACTIVE = LegalHoldStatus' "ACTIVE"

pattern LegalHoldStatus_CANCELED :: LegalHoldStatus
pattern LegalHoldStatus_CANCELED = LegalHoldStatus' "CANCELED"

pattern LegalHoldStatus_CANCELING :: LegalHoldStatus
pattern LegalHoldStatus_CANCELING = LegalHoldStatus' "CANCELING"

pattern LegalHoldStatus_CREATING :: LegalHoldStatus
pattern LegalHoldStatus_CREATING = LegalHoldStatus' "CREATING"

{-# COMPLETE
  LegalHoldStatus_ACTIVE,
  LegalHoldStatus_CANCELED,
  LegalHoldStatus_CANCELING,
  LegalHoldStatus_CREATING,
  LegalHoldStatus'
  #-}
