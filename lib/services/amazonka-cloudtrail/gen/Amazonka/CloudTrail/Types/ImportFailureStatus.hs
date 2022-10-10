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
-- Module      : Amazonka.CloudTrail.Types.ImportFailureStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.ImportFailureStatus
  ( ImportFailureStatus
      ( ..,
        ImportFailureStatus_FAILED,
        ImportFailureStatus_RETRY,
        ImportFailureStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ImportFailureStatus = ImportFailureStatus'
  { fromImportFailureStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ImportFailureStatus_FAILED :: ImportFailureStatus
pattern ImportFailureStatus_FAILED = ImportFailureStatus' "FAILED"

pattern ImportFailureStatus_RETRY :: ImportFailureStatus
pattern ImportFailureStatus_RETRY = ImportFailureStatus' "RETRY"

pattern ImportFailureStatus_SUCCEEDED :: ImportFailureStatus
pattern ImportFailureStatus_SUCCEEDED = ImportFailureStatus' "SUCCEEDED"

{-# COMPLETE
  ImportFailureStatus_FAILED,
  ImportFailureStatus_RETRY,
  ImportFailureStatus_SUCCEEDED,
  ImportFailureStatus'
  #-}
