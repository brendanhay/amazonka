{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteObjects
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
module Network.AWS.S3.DeleteObjects
    (
    -- * Request
      DeleteObjects
    -- ** Request constructor
    , deleteObjects
    -- ** Request lenses
    , dorBucket
    , dorDelete
    , dorMFA

    -- * Response
    , DeleteObjectsOutput
    -- ** Response constructor
    , deleteObjectsOutput
    -- ** Response lenses
    , dooDeleted
    , dooErrors
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data DeleteObjects = DeleteObjects
    { _dorBucket :: BucketName
    , _dorDelete :: Delete
    , _dorMFA    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorBucket' @::@ 'BucketName'
--
-- * 'dorDelete' @::@ 'Delete'
--
-- * 'dorMFA' @::@ 'Maybe' 'Text'
--
deleteObjects :: BucketName -- ^ 'dorBucket'
              -> Delete -- ^ 'dorDelete'
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { _dorBucket = p1
    , _dorDelete = p2
    , _dorMFA    = Nothing
    }

dorBucket :: Lens' DeleteObjects BucketName
dorBucket = lens _dorBucket (\s a -> s { _dorBucket = a })

dorDelete :: Lens' DeleteObjects Delete
dorDelete = lens _dorDelete (\s a -> s { _dorDelete = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
dorMFA :: Lens' DeleteObjects (Maybe Text)
dorMFA = lens _dorMFA (\s a -> s { _dorMFA = a })

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toText _dorBucket
        ]

instance ToQuery DeleteObjects where
    toQuery = const "delete"

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = mconcat
        [ "x-amz-mfa" =: _dorMFA
        ]

instance ToBody DeleteObjects where
    toBody = toBody . encodeXML . _dorDelete

data DeleteObjectsOutput = DeleteObjectsOutput
    { _dooDeleted :: [DeletedObject]
    , _dooErrors  :: [Error]
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsOutput

    request  = post
    response = const . xmlResponse $ \h x ->
        <$> x %| "Deleted"
        <*> x %| "Error"
