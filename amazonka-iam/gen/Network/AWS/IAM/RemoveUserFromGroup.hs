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

-- Module      : Network.AWS.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified user from the specified group.
module Network.AWS.IAM.RemoveUserFromGroup
    (
    -- * Request
      RemoveUserFromGroup
    -- ** Request constructor
    , removeUserFromGroup
    -- ** Request lenses
    , rufgGroupName
    , rufgUserName

    -- * Response
    , RemoveUserFromGroupResponse
    -- ** Response constructor
    , removeUserFromGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data RemoveUserFromGroup = RemoveUserFromGroup
    { _rufgGroupName :: Text
    , _rufgUserName  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveUserFromGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rufgGroupName' @::@ 'Text'
--
-- * 'rufgUserName' @::@ 'Text'
--
removeUserFromGroup :: Text -- ^ 'rufgGroupName'
                    -> Text -- ^ 'rufgUserName'
                    -> RemoveUserFromGroup
removeUserFromGroup p1 p2 = RemoveUserFromGroup
    { _rufgGroupName = p1
    , _rufgUserName  = p2
    }

-- | The name of the group to update.
rufgGroupName :: Lens' RemoveUserFromGroup Text
rufgGroupName = lens _rufgGroupName (\s a -> s { _rufgGroupName = a })

-- | The name of the user to remove.
rufgUserName :: Lens' RemoveUserFromGroup Text
rufgUserName = lens _rufgUserName (\s a -> s { _rufgUserName = a })

instance ToQuery RemoveUserFromGroup

instance ToPath RemoveUserFromGroup where
    toPath = const "/"

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RemoveUserFromGroupResponse' constructor.
removeUserFromGroupResponse :: RemoveUserFromGroupResponse
removeUserFromGroupResponse = RemoveUserFromGroupResponse

instance FromXML RemoveUserFromGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveUserFromGroupResponse"

instance AWSRequest RemoveUserFromGroup where
    type Sv RemoveUserFromGroup = IAM
    type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse

    request  = post "RemoveUserFromGroup"
    response = nullaryResponse RemoveUserFromGroupResponse
