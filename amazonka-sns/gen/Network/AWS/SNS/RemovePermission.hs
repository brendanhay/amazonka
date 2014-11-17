{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.RemovePermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes a statement from a topic's access control policy.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_RemovePermission.html>
module Network.AWS.SNS.RemovePermission
    (
    -- * Request
      RemovePermission
    -- ** Request constructor
    , removePermission
    -- ** Request lenses
    , rpLabel
    , rpTopicArn

    -- * Response
    , RemovePermissionResponse
    -- ** Response constructor
    , removePermissionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data RemovePermission = RemovePermission
    { _rpLabel    :: Text
    , _rpTopicArn :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemovePermission' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpLabel' @::@ 'Text'
--
-- * 'rpTopicArn' @::@ 'Text'
--
removePermission :: Text -- ^ 'rpTopicArn'
                 -> Text -- ^ 'rpLabel'
                 -> RemovePermission
removePermission p1 p2 = RemovePermission
    { _rpTopicArn = p1
    , _rpLabel    = p2
    }

-- | The unique label of the statement you want to remove.
rpLabel :: Lens' RemovePermission Text
rpLabel = lens _rpLabel (\s a -> s { _rpLabel = a })

-- | The ARN of the topic whose access control policy you wish to modify.
rpTopicArn :: Lens' RemovePermission Text
rpTopicArn = lens _rpTopicArn (\s a -> s { _rpTopicArn = a })

data RemovePermissionResponse = RemovePermissionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemovePermissionResponse' constructor.
removePermissionResponse :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse

instance ToPath RemovePermission where
    toPath = const "/"

instance ToQuery RemovePermission

instance ToHeaders RemovePermission

instance AWSRequest RemovePermission where
    type Sv RemovePermission = SNS
    type Rs RemovePermission = RemovePermissionResponse

    request  = post "RemovePermission"
    response = nullResponse RemovePermissionResponse
