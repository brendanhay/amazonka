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

-- Module      : Network.AWS.SQS.RemovePermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Revokes any permissions in the queue policy that matches the specified
-- Label parameter. Only the owner of the queue can remove permissions.
module Network.AWS.SQS.RemovePermission
    (
    -- * Request
      RemovePermission
    -- ** Request constructor
    , removePermission
    -- ** Request lenses
    , rpLabel
    , rpQueueUrl

    -- * Response
    , RemovePermissionResponse
    -- ** Response constructor
    , removePermissionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types

data RemovePermission = RemovePermission
    { _rpLabel    :: Text
    , _rpQueueUrl :: Text
    } (Eq, Ord, Show, Generic)

-- | 'RemovePermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpLabel' @::@ 'Text'
--
-- * 'rpQueueUrl' @::@ 'Text'
--
removePermission :: Text -- ^ 'rpQueueUrl'
                 -> Text -- ^ 'rpLabel'
                 -> RemovePermission
removePermission p1 p2 = RemovePermission
    { _rpQueueUrl = p1
    , _rpLabel    = p2
    }

-- | The identification of the permission to remove. This is the label added
-- with the AddPermission action.
rpLabel :: Lens' RemovePermission Text
rpLabel = lens _rpLabel (\s a -> s { _rpLabel = a })

-- | The URL of the Amazon SQS queue to take action on.
rpQueueUrl :: Lens' RemovePermission Text
rpQueueUrl = lens _rpQueueUrl (\s a -> s { _rpQueueUrl = a })
instance ToQuery RemovePermission

instance ToPath RemovePermission where
    toPath = const "/"

data RemovePermissionResponse = RemovePermissionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemovePermissionResponse' constructor.
removePermissionResponse :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse

instance FromXML RemovePermissionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemovePermissionResponse"

instance AWSRequest RemovePermission where
    type Sv RemovePermission = SQS
    type Rs RemovePermission = RemovePermissionResponse

    request  = post "RemovePermission"
    response = nullaryResponse RemovePermissionResponse
