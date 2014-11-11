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

-- Module      : Network.AWS.IAM.CreateGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new group. For information about the number of groups you can
-- create, see Limitations on IAM Entities in the Using IAM guide.
module Network.AWS.IAM.CreateGroup
    (
    -- * Request
      CreateGroup
    -- ** Request constructor
    , createGroup
    -- ** Request lenses
    , cgGroupName
    , cgPath

    -- * Response
    , CreateGroupResponse
    -- ** Response constructor
    , createGroupResponse
    -- ** Response lenses
    , cgrGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data CreateGroup = CreateGroup
    { _cgGroupName :: Text
    , _cgPath      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgGroupName' @::@ 'Text'
--
-- * 'cgPath' @::@ 'Maybe' 'Text'
--
createGroup :: Text -- ^ 'cgGroupName'
            -> CreateGroup
createGroup p1 = CreateGroup
    { _cgGroupName = p1
    , _cgPath      = Nothing
    }

-- | The name of the group to create. Do not include the path in this value.
cgGroupName :: Lens' CreateGroup Text
cgGroupName = lens _cgGroupName (\s a -> s { _cgGroupName = a })

-- | The path to the group. For more information about paths, see IAM
-- Identifiers in the Using IAM guide. This parameter is optional. If it is
-- not included, it defaults to a slash (/).
cgPath :: Lens' CreateGroup (Maybe Text)
cgPath = lens _cgPath (\s a -> s { _cgPath = a })
instance ToQuery CreateGroup

instance ToPath CreateGroup where
    toPath = const "/"

newtype CreateGroupResponse = CreateGroupResponse
    { _cgrGroup :: Group
    } deriving (Eq, Show, Generic)

-- | 'CreateGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cgrGroup' @::@ 'Group'
--
createGroupResponse :: Group -- ^ 'cgrGroup'
                    -> CreateGroupResponse
createGroupResponse p1 = CreateGroupResponse
    { _cgrGroup = p1
    }

-- | Information about the group.
cgrGroup :: Lens' CreateGroupResponse Group
cgrGroup = lens _cgrGroup (\s a -> s { _cgrGroup = a })
instance FromXML CreateGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateGroupResponse"

instance AWSRequest CreateGroup where
    type Sv CreateGroup = IAM
    type Rs CreateGroup = CreateGroupResponse

    request  = post "CreateGroup"
    response = xmlResponse $ \h x -> CreateGroupResponse
        <$> x %| "Group"
