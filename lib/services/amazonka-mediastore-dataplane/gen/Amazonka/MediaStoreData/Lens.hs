{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaStoreData.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaStoreData.Lens
  ( -- * Operations

    -- ** DeleteObject
    deleteObject_path,
    deleteObjectResponse_httpStatus,

    -- ** DescribeObject
    describeObject_path,
    describeObjectResponse_cacheControl,
    describeObjectResponse_contentLength,
    describeObjectResponse_contentType,
    describeObjectResponse_eTag,
    describeObjectResponse_lastModified,
    describeObjectResponse_httpStatus,

    -- ** GetObject
    getObject_range,
    getObject_path,
    getObjectResponse_cacheControl,
    getObjectResponse_contentLength,
    getObjectResponse_contentRange,
    getObjectResponse_contentType,
    getObjectResponse_eTag,
    getObjectResponse_lastModified,
    getObjectResponse_statusCode,
    getObjectResponse_body,

    -- ** ListItems
    listItems_maxResults,
    listItems_nextToken,
    listItems_path,
    listItemsResponse_items,
    listItemsResponse_nextToken,
    listItemsResponse_httpStatus,

    -- ** PutObject
    putObject_cacheControl,
    putObject_contentType,
    putObject_storageClass,
    putObject_uploadAvailability,
    putObject_path,
    putObject_body,
    putObjectResponse_contentSHA256,
    putObjectResponse_eTag,
    putObjectResponse_storageClass,
    putObjectResponse_httpStatus,

    -- * Types

    -- ** Item
    item_contentLength,
    item_contentType,
    item_eTag,
    item_lastModified,
    item_name,
    item_type,
  )
where

import Amazonka.MediaStoreData.DeleteObject
import Amazonka.MediaStoreData.DescribeObject
import Amazonka.MediaStoreData.GetObject
import Amazonka.MediaStoreData.ListItems
import Amazonka.MediaStoreData.PutObject
import Amazonka.MediaStoreData.Types.Item
