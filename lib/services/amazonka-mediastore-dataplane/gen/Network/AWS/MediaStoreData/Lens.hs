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

    -- ** PutObject
    putObject_storageClass,
    putObject_uploadAvailability,
    putObject_cacheControl,
    putObject_contentType,
    putObject_path,
    putObject_body,
    putObjectResponse_eTag,
    putObjectResponse_storageClass,
    putObjectResponse_contentSHA256,
    putObjectResponse_httpStatus,

    -- ** DeleteObject
    deleteObject_path,
    deleteObjectResponse_httpStatus,

    -- ** DescribeObject
    describeObject_path,
    describeObjectResponse_eTag,
    describeObjectResponse_contentLength,
    describeObjectResponse_cacheControl,
    describeObjectResponse_lastModified,
    describeObjectResponse_contentType,
    describeObjectResponse_httpStatus,

    -- ** GetObject
    getObject_range,
    getObject_path,
    getObjectResponse_eTag,
    getObjectResponse_contentLength,
    getObjectResponse_cacheControl,
    getObjectResponse_lastModified,
    getObjectResponse_contentRange,
    getObjectResponse_contentType,
    getObjectResponse_statusCode,
    getObjectResponse_body,

    -- ** ListItems
    listItems_path,
    listItems_nextToken,
    listItems_maxResults,
    listItemsResponse_items,
    listItemsResponse_nextToken,
    listItemsResponse_httpStatus,

    -- * Types

    -- ** Item
    item_eTag,
    item_contentLength,
    item_name,
    item_type,
    item_lastModified,
    item_contentType,
  )
where

import Network.AWS.MediaStoreData.DeleteObject
import Network.AWS.MediaStoreData.DescribeObject
import Network.AWS.MediaStoreData.GetObject
import Network.AWS.MediaStoreData.ListItems
import Network.AWS.MediaStoreData.PutObject
import Network.AWS.MediaStoreData.Types.Item
