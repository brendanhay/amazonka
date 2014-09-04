{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified Amazon Redshift parameter group. You cannot delete a
-- parameter group if it is associated with a cluster.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DeleteClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T015410Z
-- &x-amz-signedheaders=content-type;host;x-amz-date
-- 29674ca0-40da-11e2-b679-dba6cf515770.
module Network.AWS.Redshift.V2012_12_01.DeleteClusterParameterGroup
    (
    -- * Request
      DeleteClusterParameterGroup
    -- ** Request constructor
    , deleteClusterParameterGroup
    -- ** Request lenses
    , dcpgmParameterGroupName

    -- * Response
    , DeleteClusterParameterGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteClusterParameterGroup' request.
deleteClusterParameterGroup :: Text -- ^ 'dcpgmParameterGroupName'
                            -> DeleteClusterParameterGroup
deleteClusterParameterGroup p1 = DeleteClusterParameterGroup
    { _dcpgmParameterGroupName = p1
    }
{-# INLINE deleteClusterParameterGroup #-}

data DeleteClusterParameterGroup = DeleteClusterParameterGroup
    { _dcpgmParameterGroupName :: Text
      -- ^ The name of the parameter group to be deleted. Constraints: Must
      -- be the name of an existing cluster parameter group. Cannot delete
      -- a default cluster parameter group.
    } deriving (Show, Generic)

-- | The name of the parameter group to be deleted. Constraints: Must be the
-- name of an existing cluster parameter group. Cannot delete a default
-- cluster parameter group.
dcpgmParameterGroupName :: Lens' DeleteClusterParameterGroup (Text)
dcpgmParameterGroupName f x =
    f (_dcpgmParameterGroupName x)
        <&> \y -> x { _dcpgmParameterGroupName = y }
{-# INLINE dcpgmParameterGroupName #-}

instance ToQuery DeleteClusterParameterGroup where
    toQuery = genericQuery def

data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteClusterParameterGroup where
    type Sv DeleteClusterParameterGroup = Redshift
    type Rs DeleteClusterParameterGroup = DeleteClusterParameterGroupResponse

    request = post "DeleteClusterParameterGroup"
    response _ = nullaryResponse DeleteClusterParameterGroupResponse
