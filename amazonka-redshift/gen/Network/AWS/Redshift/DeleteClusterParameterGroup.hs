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

-- Module      : Network.AWS.Redshift.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a specified Amazon Redshift parameter group. You cannot delete a
-- parameter group if it is associated with a cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteClusterParameterGroup.html>
module Network.AWS.Redshift.DeleteClusterParameterGroup
    (
    -- * Request
      DeleteClusterParameterGroup
    -- ** Request constructor
    , deleteClusterParameterGroup
    -- ** Request lenses
    , dcpg1ParameterGroupName

    -- * Response
    , DeleteClusterParameterGroupResponse
    -- ** Response constructor
    , deleteClusterParameterGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup
    { _dcpg1ParameterGroupName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteClusterParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpg1ParameterGroupName' @::@ 'Text'
--
deleteClusterParameterGroup :: Text -- ^ 'dcpg1ParameterGroupName'
                            -> DeleteClusterParameterGroup
deleteClusterParameterGroup p1 = DeleteClusterParameterGroup
    { _dcpg1ParameterGroupName = p1
    }

-- | The name of the parameter group to be deleted.
--
-- Constraints:
--
-- Must be the name of an existing cluster parameter group. Cannot delete a
-- default cluster parameter group.
dcpg1ParameterGroupName :: Lens' DeleteClusterParameterGroup Text
dcpg1ParameterGroupName =
    lens _dcpg1ParameterGroupName (\s a -> s { _dcpg1ParameterGroupName = a })

data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteClusterParameterGroupResponse' constructor.
deleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse
deleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse

instance ToPath DeleteClusterParameterGroup where
    toPath = const "/"

instance ToQuery DeleteClusterParameterGroup where
    toQuery DeleteClusterParameterGroup{..} = mconcat
        [ "ParameterGroupName" =? _dcpg1ParameterGroupName
        ]

instance ToHeaders DeleteClusterParameterGroup

instance AWSRequest DeleteClusterParameterGroup where
    type Sv DeleteClusterParameterGroup = Redshift
    type Rs DeleteClusterParameterGroup = DeleteClusterParameterGroupResponse

    request  = post "DeleteClusterParameterGroup"
    response = nullResponse DeleteClusterParameterGroupResponse
