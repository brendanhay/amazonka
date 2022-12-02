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
-- Module      : Amazonka.LexV2Models.Types.ImportStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ImportStatus
  ( ImportStatus
      ( ..,
        ImportStatus_Completed,
        ImportStatus_Deleting,
        ImportStatus_Failed,
        ImportStatus_InProgress
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

pattern ImportStatus_Completed :: ImportStatus
pattern ImportStatus_Completed = ImportStatus' "Completed"

pattern ImportStatus_Deleting :: ImportStatus
pattern ImportStatus_Deleting = ImportStatus' "Deleting"

pattern ImportStatus_Failed :: ImportStatus
pattern ImportStatus_Failed = ImportStatus' "Failed"

pattern ImportStatus_InProgress :: ImportStatus
pattern ImportStatus_InProgress = ImportStatus' "InProgress"

{-# COMPLETE
  ImportStatus_Completed,
  ImportStatus_Deleting,
  ImportStatus_Failed,
  ImportStatus_InProgress,
  ImportStatus'
  #-}
