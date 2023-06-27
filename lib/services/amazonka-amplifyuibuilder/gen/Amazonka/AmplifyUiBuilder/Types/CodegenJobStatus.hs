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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenJobStatus
  ( CodegenJobStatus
      ( ..,
        CodegenJobStatus_Failed,
        CodegenJobStatus_In_progress,
        CodegenJobStatus_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CodegenJobStatus = CodegenJobStatus'
  { fromCodegenJobStatus ::
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

pattern CodegenJobStatus_Failed :: CodegenJobStatus
pattern CodegenJobStatus_Failed = CodegenJobStatus' "failed"

pattern CodegenJobStatus_In_progress :: CodegenJobStatus
pattern CodegenJobStatus_In_progress = CodegenJobStatus' "in_progress"

pattern CodegenJobStatus_Succeeded :: CodegenJobStatus
pattern CodegenJobStatus_Succeeded = CodegenJobStatus' "succeeded"

{-# COMPLETE
  CodegenJobStatus_Failed,
  CodegenJobStatus_In_progress,
  CodegenJobStatus_Succeeded,
  CodegenJobStatus'
  #-}
