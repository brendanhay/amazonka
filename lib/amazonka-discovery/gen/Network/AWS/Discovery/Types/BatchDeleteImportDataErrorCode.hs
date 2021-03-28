{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode
  ( BatchDeleteImportDataErrorCode
    ( BatchDeleteImportDataErrorCode'
    , BatchDeleteImportDataErrorCodeNotFound
    , BatchDeleteImportDataErrorCodeInternalServerError
    , BatchDeleteImportDataErrorCodeOverLimit
    , fromBatchDeleteImportDataErrorCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BatchDeleteImportDataErrorCode = BatchDeleteImportDataErrorCode'{fromBatchDeleteImportDataErrorCode
                                                                         :: Core.Text}
                                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                           Core.Generic)
                                           deriving newtype (Core.IsString, Core.Hashable,
                                                             Core.NFData, Core.ToJSONKey,
                                                             Core.FromJSONKey, Core.ToJSON,
                                                             Core.FromJSON, Core.ToXML,
                                                             Core.FromXML, Core.ToText,
                                                             Core.FromText, Core.ToByteString,
                                                             Core.ToQuery, Core.ToHeader)

pattern BatchDeleteImportDataErrorCodeNotFound :: BatchDeleteImportDataErrorCode
pattern BatchDeleteImportDataErrorCodeNotFound = BatchDeleteImportDataErrorCode' "NOT_FOUND"

pattern BatchDeleteImportDataErrorCodeInternalServerError :: BatchDeleteImportDataErrorCode
pattern BatchDeleteImportDataErrorCodeInternalServerError = BatchDeleteImportDataErrorCode' "INTERNAL_SERVER_ERROR"

pattern BatchDeleteImportDataErrorCodeOverLimit :: BatchDeleteImportDataErrorCode
pattern BatchDeleteImportDataErrorCodeOverLimit = BatchDeleteImportDataErrorCode' "OVER_LIMIT"

{-# COMPLETE 
  BatchDeleteImportDataErrorCodeNotFound,

  BatchDeleteImportDataErrorCodeInternalServerError,

  BatchDeleteImportDataErrorCodeOverLimit,
  BatchDeleteImportDataErrorCode'
  #-}
