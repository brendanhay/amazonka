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
-- Module      : Amazonka.MGN.Types.ImportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.ImportStatus
  ( ImportStatus
      ( ..,
        ImportStatus_FAILED,
        ImportStatus_PENDING,
        ImportStatus_STARTED,
        ImportStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImportStatus = ImportStatus'
  { fromImportStatus ::
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

pattern ImportStatus_FAILED :: ImportStatus
pattern ImportStatus_FAILED = ImportStatus' "FAILED"

pattern ImportStatus_PENDING :: ImportStatus
pattern ImportStatus_PENDING = ImportStatus' "PENDING"

pattern ImportStatus_STARTED :: ImportStatus
pattern ImportStatus_STARTED = ImportStatus' "STARTED"

pattern ImportStatus_SUCCEEDED :: ImportStatus
pattern ImportStatus_SUCCEEDED = ImportStatus' "SUCCEEDED"

{-# COMPLETE
  ImportStatus_FAILED,
  ImportStatus_PENDING,
  ImportStatus_STARTED,
  ImportStatus_SUCCEEDED,
  ImportStatus'
  #-}
