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
    , deleteGroup
    -- ** Request lenses
    , dgrGroupName

    -- * Response
    , DeleteGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteGroup' request.
deleteGroup :: Text -- ^ 'dgrGroupName'
            -> DeleteGroup
deleteGroup p1 = DeleteGroup
    { _dgrGroupName = p1
    }

data DeleteGroup = DeleteGroup
    { _dgrGroupName :: Text
      -- ^ Name of the group to delete.
    } deriving (Show, Generic)

-- | Name of the group to delete.
dgrGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteGroup
    -> f DeleteGroup
dgrGroupName f x =
    (\y -> x { _dgrGroupName = y })
       <$> f (_dgrGroupName x)
{-# INLINE dgrGroupName #-}

instance ToQuery DeleteGroup where
    toQuery = genericQuery def

data DeleteGroupResponse = DeleteGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteGroup where
    type Sv DeleteGroup = IAM
    type Rs DeleteGroup = DeleteGroupResponse

    request = post "DeleteGroup"
    response _ = nullaryResponse DeleteGroupResponse
