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
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionResultCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionResultCode
  ( ExecutionResultCode
      ( ..,
        ExecutionResultCode_PARSING_FAILED,
        ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ExecutionResultCode = ExecutionResultCode'
  { fromExecutionResultCode ::
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

pattern ExecutionResultCode_PARSING_FAILED :: ExecutionResultCode
pattern ExecutionResultCode_PARSING_FAILED = ExecutionResultCode' "PARSING_FAILED"

pattern ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED :: ExecutionResultCode
pattern ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED = ExecutionResultCode' "VPC_ENDPOINT_SETUP_FAILED"

{-# COMPLETE
  ExecutionResultCode_PARSING_FAILED,
  ExecutionResultCode_VPC_ENDPOINT_SETUP_FAILED,
  ExecutionResultCode'
  #-}
