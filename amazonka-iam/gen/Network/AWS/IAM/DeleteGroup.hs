{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified group. The group must not contain any users or have
-- any attached policies.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroup.html>
module Network.AWS.IAM.DeleteGroup
    (
    -- * Request
      DeleteGroup
    -- ** Request constructor
    , deleteGroup
    -- ** Request lenses
    , dgGroupName

    -- * Response
    , DeleteGroupResponse
    -- ** Response constructor
    , deleteGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype DeleteGroup = DeleteGroup
    { _dgGroupName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgGroupName' @::@ 'Text'
--
deleteGroup :: Text -- ^ 'dgGroupName'
            -> DeleteGroup
deleteGroup p1 = DeleteGroup
    { _dgGroupName = p1
    }

-- | The name of the group to delete.
dgGroupName :: Lens' DeleteGroup Text
dgGroupName = lens _dgGroupName (\s a -> s { _dgGroupName = a })

data DeleteGroupResponse = DeleteGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteGroupResponse' constructor.
deleteGroupResponse :: DeleteGroupResponse
deleteGroupResponse = DeleteGroupResponse

instance ToPath DeleteGroup where
    toPath = const "/"

instance ToQuery DeleteGroup where
    toQuery DeleteGroup{..} = mconcat
        [ "GroupName" =? _dgGroupName
        ]

instance ToHeaders DeleteGroup

instance AWSRequest DeleteGroup where
    type Sv DeleteGroup = IAM
    type Rs DeleteGroup = DeleteGroupResponse

    request  = post "DeleteGroup"
    response = nullResponse DeleteGroupResponse
