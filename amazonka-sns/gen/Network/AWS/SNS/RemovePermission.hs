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
module Network.AWS.SNS.RemovePermission
    (
    -- * Request
      RemovePermissionInput
    -- ** Request constructor
    , removePermissionInput
    -- ** Request lenses
    , rpiLabel
    , rpiTopicArn

    -- * Response
    , RemovePermissionResponse
    -- ** Response constructor
    , removePermissionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data RemovePermissionInput = RemovePermissionInput
    { _rpiLabel    :: Text
    , _rpiTopicArn :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemovePermissionInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpiLabel' @::@ 'Text'
--
-- * 'rpiTopicArn' @::@ 'Text'
--
removePermissionInput :: Text -- ^ 'rpiTopicArn'
                      -> Text -- ^ 'rpiLabel'
                      -> RemovePermissionInput
removePermissionInput p1 p2 = RemovePermissionInput
    { _rpiTopicArn = p1
    , _rpiLabel    = p2
    }

-- | The unique label of the statement you want to remove.
rpiLabel :: Lens' RemovePermissionInput Text
rpiLabel = lens _rpiLabel (\s a -> s { _rpiLabel = a })

-- | The ARN of the topic whose access control policy you wish to modify.
rpiTopicArn :: Lens' RemovePermissionInput Text
rpiTopicArn = lens _rpiTopicArn (\s a -> s { _rpiTopicArn = a })

instance ToPath RemovePermissionInput where
    toPath = const "/"

instance ToQuery RemovePermissionInput

data RemovePermissionResponse = RemovePermissionResponse

-- | 'RemovePermissionResponse' constructor.
removePermissionResponse :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse

instance AWSRequest RemovePermissionInput where
    type Sv RemovePermissionInput = SNS
    type Rs RemovePermissionInput = RemovePermissionResponse

    request  = post "RemovePermission"
    response = const (nullaryResponse RemovePermissionResponse)
