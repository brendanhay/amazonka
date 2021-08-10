{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStoreData.Lens
  ( -- * Operations

    -- ** ListItems
    listItems_nextToken,
    listItems_maxResults,
    listItems_path,
    listItemsResponse_nextToken,
    listItemsResponse_items,
    listItemsResponse_httpStatus,

    -- ** DeleteObject
    deleteObject_path,
    deleteObjectResponse_httpStatus,

    -- ** DescribeObject
    describeObject_path,
    describeObjectResponse_eTag,
    describeObjectResponse_contentType,
    describeObjectResponse_contentLength,
    describeObjectResponse_lastModified,
    describeObjectResponse_cacheControl,
    describeObjectResponse_httpStatus,

    -- ** PutObject
    putObject_contentType,
    putObject_storageClass,
    putObject_cacheControl,
    putObject_uploadAvailability,
    putObject_path,
    putObject_body,
    putObjectResponse_eTag,
    putObjectResponse_contentSHA256,
    putObjectResponse_storageClass,
    putObjectResponse_httpStatus,

    -- ** GetObject
    getObject_range,
    getObject_path,
    getObjectResponse_eTag,
    getObjectResponse_contentType,
    getObjectResponse_contentRange,
    getObjectResponse_contentLength,
    getObjectResponse_lastModified,
    getObjectResponse_cacheControl,
    getObjectResponse_statusCode,
    getObjectResponse_body,

    -- * Types

    -- ** Item
    item_eTag,
    item_contentType,
    item_contentLength,
    item_name,
    item_lastModified,
    item_type,
  )
where

import Network.AWS.MediaStoreData.DeleteObject
import Network.AWS.MediaStoreData.DescribeObject
import Network.AWS.MediaStoreData.GetObject
import Network.AWS.MediaStoreData.ListItems
import Network.AWS.MediaStoreData.PutObject
import Network.AWS.MediaStoreData.Types.Item
