{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified DBParameterGroup. The DBParameterGroup to be deleted
-- cannot be associated with any DB instances.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBParameterGroup.html>
module Network.AWS.RDS.DeleteDBParameterGroup
    (
    -- * Request
      DeleteDBParameterGroup
    -- ** Request constructor
    , deleteDBParameterGroup
    -- ** Request lenses
    , ddbpg1DBParameterGroupName

    -- * Response
    , DeleteDBParameterGroupResponse
    -- ** Response constructor
    , deleteDBParameterGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

newtype DeleteDBParameterGroup = DeleteDBParameterGroup
    { _ddbpg1DBParameterGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteDBParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbpg1DBParameterGroupName' @::@ 'Text'
--
deleteDBParameterGroup :: Text -- ^ 'ddbpg1DBParameterGroupName'
                       -> DeleteDBParameterGroup
deleteDBParameterGroup p1 = DeleteDBParameterGroup
    { _ddbpg1DBParameterGroupName = p1
    }

-- | The name of the DB parameter group. Constraints: Must be the name of an
-- existing DB parameter group You cannot delete a default DB parameter
-- group Cannot be associated with any DB instances.
ddbpg1DBParameterGroupName :: Lens' DeleteDBParameterGroup Text
ddbpg1DBParameterGroupName =
    lens _ddbpg1DBParameterGroupName
        (\s a -> s { _ddbpg1DBParameterGroupName = a })

data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDBParameterGroupResponse' constructor.
deleteDBParameterGroupResponse :: DeleteDBParameterGroupResponse
deleteDBParameterGroupResponse = DeleteDBParameterGroupResponse

instance AWSRequest DeleteDBParameterGroup where
    type Sv DeleteDBParameterGroup = RDS
    type Rs DeleteDBParameterGroup = DeleteDBParameterGroupResponse

    request  = post "DeleteDBParameterGroup"
    response = nullResponse DeleteDBParameterGroupResponse

instance ToPath DeleteDBParameterGroup where
    toPath = const "/"

instance ToHeaders DeleteDBParameterGroup

instance ToQuery DeleteDBParameterGroup
