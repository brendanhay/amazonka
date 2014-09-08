{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified group. The group must not contain any users or have
-- any attached policies. https://iam.amazonaws.com/ ?Action=DeleteGroup
-- &Group=Test &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeleteGroup
    (
    -- * Request
      DeleteGroup
    -- ** Request constructor
    , mkDeleteGroup
    -- ** Request lenses
    , dgGroupName

    -- * Response
    , DeleteGroupResponse
    -- ** Response constructor
    , mkDeleteGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
newtype DeleteGroup = DeleteGroup
    { _dgGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGroup' request.
mkDeleteGroup :: Text -- ^ 'dgGroupName'
              -> DeleteGroup
mkDeleteGroup p1 = DeleteGroup
    { _dgGroupName = p1
    }

-- | Name of the group to delete.
dgGroupName :: Lens' DeleteGroup Text
dgGroupName = lens _dgGroupName (\s a -> s { _dgGroupName = a })

instance ToQuery DeleteGroup where
    toQuery = genericQuery def

data DeleteGroupResponse = DeleteGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteGroupResponse :: DeleteGroupResponse
mkDeleteGroupResponse = DeleteGroupResponse

instance AWSRequest DeleteGroup where
    type Sv DeleteGroup = IAM
    type Rs DeleteGroup = DeleteGroupResponse

    request = post "DeleteGroup"
    response _ = nullaryResponse DeleteGroupResponse
