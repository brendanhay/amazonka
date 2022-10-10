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
-- Module      : Amazonka.DeviceFarm.Types.ExecutionResultCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.ExecutionResultCode
  ( ExecutionResultCode
      ( ..,
        ExecutionResultCode_PARSING_FAILED,
        ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ExecutionResultCode = ExecutionResultCode'
  { fromExecutionResultCode ::
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

pattern ExecutionResultCode_PARSING_FAILED :: ExecutionResultCode
pattern ExecutionResultCode_PARSING_FAILED = ExecutionResultCode' "PARSING_FAILED"

pattern ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED :: ExecutionResultCode
pattern ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED = ExecutionResultCode' "VPC_ENDPOINT_SETUP_FAILED"

{-# COMPLETE
  ExecutionResultCode_PARSING_FAILED,
  ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED,
  ExecutionResultCode'
  #-}
