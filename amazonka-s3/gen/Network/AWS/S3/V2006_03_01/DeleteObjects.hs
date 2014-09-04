{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.DeleteObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation enables you to delete multiple objects from a bucket using a
-- single HTTP request. You may specify up to 1000 keys.
module Network.AWS.S3.V2006_03_01.DeleteObjects
    (
    -- * Request
      DeleteObjects
    -- ** Request constructor
    , deleteObjects
    -- ** Request lenses
    , dosDelete
    , dosBucket
    , dosMFA

    -- * Response
    , DeleteObjectsResponse
    -- ** Response lenses
    , dopDeleted
    , dopErrors
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

type DeleteMultipleObjects = DeleteObjects

-- | Minimum specification for a 'DeleteObjects' request.
deleteObjects :: Delete -- ^ 'dosDelete'
              -> BucketName -- ^ 'dosBucket'
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { _dosDelete = p1
    , _dosBucket = p2
    , _dosMFA = Nothing
    }
{-# INLINE deleteObjects #-}

data DeleteObjects = DeleteObjects
    { _dosDelete :: Delete
    , _dosBucket :: BucketName
    , _dosMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    } deriving (Show, Generic)

dosDelete :: Lens' DeleteObjects (Delete)
dosDelete f x =
    f (_dosDelete x)
        <&> \y -> x { _dosDelete = y }
{-# INLINE dosDelete #-}

dosBucket :: Lens' DeleteObjects (BucketName)
dosBucket f x =
    f (_dosBucket x)
        <&> \y -> x { _dosBucket = y }
{-# INLINE dosBucket #-}

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
dosMFA :: Lens' DeleteObjects (Maybe Text)
dosMFA f x =
    f (_dosMFA x)
        <&> \y -> x { _dosMFA = y }
{-# INLINE dosMFA #-}

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toBS _dosBucket
        ]

instance ToQuery DeleteObjects where
    toQuery DeleteObjects{..} = mconcat
        [ "delete"
        ]

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = concat
        [ "x-amz-mfa" =: _dosMFA
        ]

instance ToBody DeleteObjects where
    toBody = toBody . encodeXML . _dosDelete

data DeleteObjectsResponse = DeleteObjectsResponse
    { _dopDeleted :: [DeletedObject]
    , _dopErrors :: [Error]
    } deriving (Show, Generic)

dopDeleted :: Lens' DeleteObjectsResponse ([DeletedObject])
dopDeleted f x =
    f (_dopDeleted x)
        <&> \y -> x { _dopDeleted = y }
{-# INLINE dopDeleted #-}

dopErrors :: Lens' DeleteObjectsResponse ([Error])
dopErrors f x =
    f (_dopErrors x)
        <&> \y -> x { _dopErrors = y }
{-# INLINE dopErrors #-}

instance FromXML DeleteObjectsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsResponse

    request = post
    response _ = xmlResponse
