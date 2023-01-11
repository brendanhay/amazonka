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
-- Module      : Amazonka.DynamoDB.Types.ImportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ImportStatus
  ( ImportStatus
      ( ..,
        ImportStatus_CANCELLED,
        ImportStatus_CANCELLING,
        ImportStatus_COMPLETED,
        ImportStatus_FAILED,
        ImportStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
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

pattern ImportStatus_CANCELLED :: ImportStatus
pattern ImportStatus_CANCELLED = ImportStatus' "CANCELLED"

pattern ImportStatus_CANCELLING :: ImportStatus
pattern ImportStatus_CANCELLING = ImportStatus' "CANCELLING"

pattern ImportStatus_COMPLETED :: ImportStatus
pattern ImportStatus_COMPLETED = ImportStatus' "COMPLETED"

pattern ImportStatus_FAILED :: ImportStatus
pattern ImportStatus_FAILED = ImportStatus' "FAILED"

pattern ImportStatus_IN_PROGRESS :: ImportStatus
pattern ImportStatus_IN_PROGRESS = ImportStatus' "IN_PROGRESS"

{-# COMPLETE
  ImportStatus_CANCELLED,
  ImportStatus_CANCELLING,
  ImportStatus_COMPLETED,
  ImportStatus_FAILED,
  ImportStatus_IN_PROGRESS,
  ImportStatus'
  #-}
