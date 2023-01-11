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
-- Module      : Amazonka.MigrationHubStrategy.Types.ImportFileTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ImportFileTaskStatus
  ( ImportFileTaskStatus
      ( ..,
        ImportFileTaskStatus_DeleteFailed,
        ImportFileTaskStatus_DeleteInProgress,
        ImportFileTaskStatus_DeletePartialSuccess,
        ImportFileTaskStatus_DeleteSuccess,
        ImportFileTaskStatus_ImportFailed,
        ImportFileTaskStatus_ImportInProgress,
        ImportFileTaskStatus_ImportPartialSuccess,
        ImportFileTaskStatus_ImportSuccess
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImportFileTaskStatus = ImportFileTaskStatus'
  { fromImportFileTaskStatus ::
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

pattern ImportFileTaskStatus_DeleteFailed :: ImportFileTaskStatus
pattern ImportFileTaskStatus_DeleteFailed = ImportFileTaskStatus' "DeleteFailed"

pattern ImportFileTaskStatus_DeleteInProgress :: ImportFileTaskStatus
pattern ImportFileTaskStatus_DeleteInProgress = ImportFileTaskStatus' "DeleteInProgress"

pattern ImportFileTaskStatus_DeletePartialSuccess :: ImportFileTaskStatus
pattern ImportFileTaskStatus_DeletePartialSuccess = ImportFileTaskStatus' "DeletePartialSuccess"

pattern ImportFileTaskStatus_DeleteSuccess :: ImportFileTaskStatus
pattern ImportFileTaskStatus_DeleteSuccess = ImportFileTaskStatus' "DeleteSuccess"

pattern ImportFileTaskStatus_ImportFailed :: ImportFileTaskStatus
pattern ImportFileTaskStatus_ImportFailed = ImportFileTaskStatus' "ImportFailed"

pattern ImportFileTaskStatus_ImportInProgress :: ImportFileTaskStatus
pattern ImportFileTaskStatus_ImportInProgress = ImportFileTaskStatus' "ImportInProgress"

pattern ImportFileTaskStatus_ImportPartialSuccess :: ImportFileTaskStatus
pattern ImportFileTaskStatus_ImportPartialSuccess = ImportFileTaskStatus' "ImportPartialSuccess"

pattern ImportFileTaskStatus_ImportSuccess :: ImportFileTaskStatus
pattern ImportFileTaskStatus_ImportSuccess = ImportFileTaskStatus' "ImportSuccess"

{-# COMPLETE
  ImportFileTaskStatus_DeleteFailed,
  ImportFileTaskStatus_DeleteInProgress,
  ImportFileTaskStatus_DeletePartialSuccess,
  ImportFileTaskStatus_DeleteSuccess,
  ImportFileTaskStatus_ImportFailed,
  ImportFileTaskStatus_ImportInProgress,
  ImportFileTaskStatus_ImportPartialSuccess,
  ImportFileTaskStatus_ImportSuccess,
  ImportFileTaskStatus'
  #-}
