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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyValueHistoryErrorCode
  ( BatchGetAssetPropertyValueHistoryErrorCode
      ( ..,
        BatchGetAssetPropertyValueHistoryErrorCode_AccessDeniedException,
        BatchGetAssetPropertyValueHistoryErrorCode_InvalidRequestException,
        BatchGetAssetPropertyValueHistoryErrorCode_ResourceNotFoundException
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype BatchGetAssetPropertyValueHistoryErrorCode = BatchGetAssetPropertyValueHistoryErrorCode'
  { fromBatchGetAssetPropertyValueHistoryErrorCode ::
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

pattern BatchGetAssetPropertyValueHistoryErrorCode_AccessDeniedException :: BatchGetAssetPropertyValueHistoryErrorCode
pattern BatchGetAssetPropertyValueHistoryErrorCode_AccessDeniedException = BatchGetAssetPropertyValueHistoryErrorCode' "AccessDeniedException"

pattern BatchGetAssetPropertyValueHistoryErrorCode_InvalidRequestException :: BatchGetAssetPropertyValueHistoryErrorCode
pattern BatchGetAssetPropertyValueHistoryErrorCode_InvalidRequestException = BatchGetAssetPropertyValueHistoryErrorCode' "InvalidRequestException"

pattern BatchGetAssetPropertyValueHistoryErrorCode_ResourceNotFoundException :: BatchGetAssetPropertyValueHistoryErrorCode
pattern BatchGetAssetPropertyValueHistoryErrorCode_ResourceNotFoundException = BatchGetAssetPropertyValueHistoryErrorCode' "ResourceNotFoundException"

{-# COMPLETE
  BatchGetAssetPropertyValueHistoryErrorCode_AccessDeniedException,
  BatchGetAssetPropertyValueHistoryErrorCode_InvalidRequestException,
  BatchGetAssetPropertyValueHistoryErrorCode_ResourceNotFoundException,
  BatchGetAssetPropertyValueHistoryErrorCode'
  #-}
