{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

-- | Deletes the specified group. The group must not contain any users or have
-- any attached policies. https://iam.amazonaws.com/ ?Action=DeleteGroup
-- &Group=Test &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
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

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteGroup = DeleteGroup
    { _dgGroupName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
deleteGroup :: Text -- ^ 'dgGroupName'
            -> DeleteGroup
deleteGroup p1 = DeleteGroup
    { _dgGroupName = p1
    }

-- | Name of the group to delete.
dgGroupName :: Lens' DeleteGroup Text
dgGroupName = lens _dgGroupName (\s a -> s { _dgGroupName = a })

instance ToQuery DeleteGroup where
    toQuery = genericQuery def

data DeleteGroupResponse = DeleteGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteGroupResponse :: DeleteGroupResponse
deleteGroupResponse = DeleteGroupResponse

instance AWSRequest DeleteGroup where
    type Sv DeleteGroup = IAM
    type Rs DeleteGroup = DeleteGroupResponse

    request = post "DeleteGroup"
    response _ = nullaryResponse DeleteGroupResponse
