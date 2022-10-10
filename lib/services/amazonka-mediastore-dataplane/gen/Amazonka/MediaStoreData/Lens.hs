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
    describeObjectResponse_contentLength,
    describeObjectResponse_lastModified,
    describeObjectResponse_cacheControl,
    describeObjectResponse_eTag,
    describeObjectResponse_contentType,
    describeObjectResponse_httpStatus,

    -- ** GetObject
    getObject_range,
    getObject_path,
    getObjectResponse_contentLength,
    getObjectResponse_contentRange,
    getObjectResponse_lastModified,
    getObjectResponse_cacheControl,
    getObjectResponse_eTag,
    getObjectResponse_contentType,
    getObjectResponse_statusCode,
    getObjectResponse_body,

    -- ** ListItems
    listItems_nextToken,
    listItems_path,
    listItems_maxResults,
    listItemsResponse_items,
    listItemsResponse_nextToken,
    listItemsResponse_httpStatus,

    -- ** PutObject
    putObject_uploadAvailability,
    putObject_cacheControl,
    putObject_storageClass,
    putObject_contentType,
    putObject_path,
    putObject_body,
    putObjectResponse_storageClass,
    putObjectResponse_eTag,
    putObjectResponse_contentSHA256,
    putObjectResponse_httpStatus,

    -- * Types

    -- ** Item
    item_name,
    item_type,
    item_contentLength,
    item_lastModified,
    item_eTag,
    item_contentType,
  )
where

import Amazonka.MediaStoreData.DeleteObject
import Amazonka.MediaStoreData.DescribeObject
import Amazonka.MediaStoreData.GetObject
import Amazonka.MediaStoreData.ListItems
import Amazonka.MediaStoreData.PutObject
import Amazonka.MediaStoreData.Types.Item
