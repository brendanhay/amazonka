{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift
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
module Network.AWS.Redshift
    (
    -- * Request
      DeleteClusterParameterGroup
    -- ** Request constructor
    , mkDeleteClusterParameterGroup
    -- ** Request lenses
    , dcpgParameterGroupName

    -- * Response
    , DeleteClusterParameterGroupResponse
    -- ** Response constructor
    , mkDeleteClusterParameterGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup
    { _dcpgParameterGroupName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteClusterParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Text@
--
mkDeleteClusterParameterGroup :: Text -- ^ 'dcpgParameterGroupName'
                              -> DeleteClusterParameterGroup
mkDeleteClusterParameterGroup p1 = DeleteClusterParameterGroup
    { _dcpgParameterGroupName = p1
    }

-- | The name of the parameter group to be deleted. Constraints: Must be the
-- name of an existing cluster parameter group. Cannot delete a default
-- cluster parameter group.
dcpgParameterGroupName :: Lens' DeleteClusterParameterGroup Text
dcpgParameterGroupName =
    lens _dcpgParameterGroupName (\s a -> s { _dcpgParameterGroupName = a })

instance ToQuery DeleteClusterParameterGroup where
    toQuery = genericQuery def

data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteClusterParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse
mkDeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse

instance AWSRequest DeleteClusterParameterGroup where
    type Sv DeleteClusterParameterGroup = Redshift
    type Rs DeleteClusterParameterGroup = DeleteClusterParameterGroupResponse

    request = post "DeleteClusterParameterGroup"
    response _ = nullaryResponse DeleteClusterParameterGroupResponse
