{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , doBucket
    , doDelete
    , doMFA

    -- * Response
    , DeleteObjectsOutput
    -- ** Response constructor
    , deleteObjectsResponse
    -- ** Response lenses
    , dooDeleted
    , dooErrors
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data DeleteObjects = DeleteObjects
    { _doBucket :: Text
    , _doDelete :: Delete
    , _doMFA    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DeleteObjects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'doBucket' @::@ 'Text'
--
-- * 'doDelete' @::@ 'Delete'
--
-- * 'doMFA' @::@ 'Maybe' 'Text'
--
deleteObjects :: Text -- ^ 'doBucket'
              -> Delete -- ^ 'doDelete'
              -> DeleteObjects
deleteObjects p1 p2 = DeleteObjects
    { _doBucket = p1
    , _doDelete = p2
    , _doMFA    = Nothing
    }

doBucket :: Lens' DeleteObjects Text
doBucket = lens _doBucket (\s a -> s { _doBucket = a })

doDelete :: Lens' DeleteObjects Delete
doDelete = lens _doDelete (\s a -> s { _doDelete = a })

-- | The concatenation of the authentication device's serial number, a space,
-- and the value that is displayed on your authentication device.
doMFA :: Lens' DeleteObjects (Maybe Text)
doMFA = lens _doMFA (\s a -> s { _doMFA = a })

instance ToPath DeleteObjects where
    toPath DeleteObjects{..} = mconcat
        [ "/"
        , toText _doBucket
        ]

instance ToQuery DeleteObjects where
    toQuery = const "delete"

instance ToHeaders DeleteObjects where
    toHeaders DeleteObjects{..} = mconcat
        [ "x-amz-mfa" =: _doMFA
        ]

instance ToBody DeleteObjects where
    toBody = toBody . encodeXML . _doDelete

data DeleteObjectsOutput = DeleteObjectsOutput
    { _dooDeleted :: [S3ServiceError]
    , _dooErrors  :: [S3ServiceError]
    } deriving (Eq, Show, Generic)

-- | 'DeleteObjectsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dooDeleted' @::@ ['S3ServiceError']
--
-- * 'dooErrors' @::@ ['S3ServiceError']
--
deleteObjectsResponse :: DeleteObjectsOutput
deleteObjectsResponse = DeleteObjectsOutput
    { _dooDeleted = mempty
    , _dooErrors  = mempty
    }

dooDeleted :: Lens' DeleteObjectsOutput [S3ServiceError]
dooDeleted = lens _dooDeleted (\s a -> s { _dooDeleted = a })

dooErrors :: Lens' DeleteObjectsOutput [S3ServiceError]
dooErrors = lens _dooErrors (\s a -> s { _dooErrors = a })

instance AWSRequest DeleteObjects where
    type Sv DeleteObjects = S3
    type Rs DeleteObjects = DeleteObjectsOutput

    request  = post
    response = xmlResponse $ \h x -> DeleteObjectsOutput
        <$> x %| "Deleted"
        <*> x %| "Error"
