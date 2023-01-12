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
-- Module      : Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchGetAssetPropertyAggregatesErrorCode
  ( BatchGetAssetPropertyAggregatesErrorCode
      ( ..,
        BatchGetAssetPropertyAggregatesErrorCode_AccessDeniedException,
        BatchGetAssetPropertyAggregatesErrorCode_InvalidRequestException,
        BatchGetAssetPropertyAggregatesErrorCode_ResourceNotFoundException
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BatchGetAssetPropertyAggregatesErrorCode = BatchGetAssetPropertyAggregatesErrorCode'
  { fromBatchGetAssetPropertyAggregatesErrorCode ::
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

pattern BatchGetAssetPropertyAggregatesErrorCode_AccessDeniedException :: BatchGetAssetPropertyAggregatesErrorCode
pattern BatchGetAssetPropertyAggregatesErrorCode_AccessDeniedException = BatchGetAssetPropertyAggregatesErrorCode' "AccessDeniedException"

pattern BatchGetAssetPropertyAggregatesErrorCode_InvalidRequestException :: BatchGetAssetPropertyAggregatesErrorCode
pattern BatchGetAssetPropertyAggregatesErrorCode_InvalidRequestException = BatchGetAssetPropertyAggregatesErrorCode' "InvalidRequestException"

pattern BatchGetAssetPropertyAggregatesErrorCode_ResourceNotFoundException :: BatchGetAssetPropertyAggregatesErrorCode
pattern BatchGetAssetPropertyAggregatesErrorCode_ResourceNotFoundException = BatchGetAssetPropertyAggregatesErrorCode' "ResourceNotFoundException"

{-# COMPLETE
  BatchGetAssetPropertyAggregatesErrorCode_AccessDeniedException,
  BatchGetAssetPropertyAggregatesErrorCode_InvalidRequestException,
  BatchGetAssetPropertyAggregatesErrorCode_ResourceNotFoundException,
  BatchGetAssetPropertyAggregatesErrorCode'
  #-}
