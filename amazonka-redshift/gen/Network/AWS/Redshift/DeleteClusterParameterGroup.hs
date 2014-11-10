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

-- Module      : Network.AWS.Redshift.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified Amazon Redshift parameter group.
module Network.AWS.Redshift.DeleteClusterParameterGroup
    (
    -- * Request
      DeleteClusterParameterGroupMessage
    -- ** Request constructor
    , deleteClusterParameterGroup
    -- ** Request lenses
    , dcpgmParameterGroupName

    -- * Response
    , DeleteClusterParameterGroupResponse
    -- ** Response constructor
    , deleteClusterParameterGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DeleteClusterParameterGroupMessage = DeleteClusterParameterGroupMessage
    { _dcpgmParameterGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteClusterParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgmParameterGroupName' @::@ 'Text'
--
deleteClusterParameterGroup :: Text -- ^ 'dcpgmParameterGroupName'
                            -> DeleteClusterParameterGroupMessage
deleteClusterParameterGroup p1 = DeleteClusterParameterGroupMessage
    { _dcpgmParameterGroupName = p1
    }

-- | The name of the parameter group to be deleted. Constraints: Must be the
-- name of an existing cluster parameter group. Cannot delete a default
-- cluster parameter group.
dcpgmParameterGroupName :: Lens' DeleteClusterParameterGroupMessage Text
dcpgmParameterGroupName =
    lens _dcpgmParameterGroupName (\s a -> s { _dcpgmParameterGroupName = a })

instance ToPath DeleteClusterParameterGroupMessage where
    toPath = const "/"

instance ToQuery DeleteClusterParameterGroupMessage

data DeleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse

-- | 'DeleteClusterParameterGroupResponse' constructor.
deleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse
deleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse

instance AWSRequest DeleteClusterParameterGroupMessage where
    type Sv DeleteClusterParameterGroupMessage = Redshift
    type Rs DeleteClusterParameterGroupMessage = DeleteClusterParameterGroupResponse

    request  = post "DeleteClusterParameterGroup"
    response = const (nullaryResponse DeleteClusterParameterGroupResponse)
