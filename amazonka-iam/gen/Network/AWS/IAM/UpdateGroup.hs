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

-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the name and/or the path of the specified group. You should
-- understand the implications of changing a group's path or name. For more
-- information, see Renaming Users and Groups in the Using IAM guide.
module Network.AWS.IAM.UpdateGroup
    (
    -- * Request
      UpdateGroup
    -- ** Request constructor
    , updateGroup
    -- ** Request lenses
    , ugGroupName
    , ugNewGroupName
    , ugNewPath

    -- * Response
    , UpdateGroupResponse
    -- ** Response constructor
    , updateGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data UpdateGroup = UpdateGroup
    { _ugGroupName    :: Text
    , _ugNewGroupName :: Maybe Text
    , _ugNewPath      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ugGroupName' @::@ 'Text'
--
-- * 'ugNewGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ugNewPath' @::@ 'Maybe' 'Text'
--
updateGroup :: Text -- ^ 'ugGroupName'
            -> UpdateGroup
updateGroup p1 = UpdateGroup
    { _ugGroupName    = p1
    , _ugNewPath      = Nothing
    , _ugNewGroupName = Nothing
    }

-- | Name of the group to update. If you're changing the name of the group,
-- this is the original name.
ugGroupName :: Lens' UpdateGroup Text
ugGroupName = lens _ugGroupName (\s a -> s { _ugGroupName = a })

-- | New name for the group. Only include this if changing the group's name.
ugNewGroupName :: Lens' UpdateGroup (Maybe Text)
ugNewGroupName = lens _ugNewGroupName (\s a -> s { _ugNewGroupName = a })

-- | New path for the group. Only include this if changing the group's path.
ugNewPath :: Lens' UpdateGroup (Maybe Text)
ugNewPath = lens _ugNewPath (\s a -> s { _ugNewPath = a })

instance ToQuery UpdateGroup

instance ToPath UpdateGroup where
    toPath = const "/"

data UpdateGroupResponse = UpdateGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateGroupResponse' constructor.
updateGroupResponse :: UpdateGroupResponse
updateGroupResponse = UpdateGroupResponse

instance FromXML UpdateGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateGroupResponse"

instance AWSRequest UpdateGroup where
    type Sv UpdateGroup = IAM
    type Rs UpdateGroup = UpdateGroupResponse

    request  = post "UpdateGroup"
    response = nullaryResponse UpdateGroupResponse
