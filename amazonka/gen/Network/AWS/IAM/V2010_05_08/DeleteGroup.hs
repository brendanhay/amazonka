{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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
module Network.AWS.IAM.V2010_05_08.DeleteGroup where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data DeleteGroup = DeleteGroup
    { _dgrGroupName :: Text
      -- ^ Name of the group to delete.
    } deriving (Generic)

instance ToQuery DeleteGroup where
    toQuery = genericToQuery def

instance AWSRequest DeleteGroup where
    type Sv DeleteGroup = IAM
    type Rs DeleteGroup = DeleteGroupResponse

    request = post "DeleteGroup"
    response _ _ = return (Right DeleteGroupResponse)

data DeleteGroupResponse = DeleteGroupResponse
    deriving (Eq, Show, Generic)
