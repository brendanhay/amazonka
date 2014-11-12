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
module Network.AWS.RDS.DeleteDBParameterGroup
    (
    -- * Request
      DeleteDBParameterGroupMessage
    -- ** Request constructor
    , deleteDBParameterGroup
    -- ** Request lenses
    , ddbpgm1DBParameterGroupName

    -- * Response
    , DeleteDBParameterGroupResponse
    -- ** Response constructor
    , deleteDBParameterGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

newtype DeleteDBParameterGroupMessage = DeleteDBParameterGroupMessage
    { _ddbpgm1DBParameterGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteDBParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbpgm1DBParameterGroupName' @::@ 'Text'
--
deleteDBParameterGroup :: Text -- ^ 'ddbpgm1DBParameterGroupName'
                       -> DeleteDBParameterGroupMessage
deleteDBParameterGroup p1 = DeleteDBParameterGroupMessage
    { _ddbpgm1DBParameterGroupName = p1
    }

-- | The name of the DB parameter group. Constraints: Must be the name of an
-- existing DB parameter group You cannot delete a default DB parameter
-- group Cannot be associated with any DB instances.
ddbpgm1DBParameterGroupName :: Lens' DeleteDBParameterGroupMessage Text
ddbpgm1DBParameterGroupName =
    lens _ddbpgm1DBParameterGroupName
        (\s a -> s { _ddbpgm1DBParameterGroupName = a })

instance ToQuery DeleteDBParameterGroupMessage

instance ToPath DeleteDBParameterGroupMessage where
    toPath = const "/"

data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDBParameterGroupResponse' constructor.
deleteDBParameterGroupResponse :: DeleteDBParameterGroupResponse
deleteDBParameterGroupResponse = DeleteDBParameterGroupResponse

instance FromXML DeleteDBParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteDBParameterGroupResponse"

instance AWSRequest DeleteDBParameterGroupMessage where
    type Sv DeleteDBParameterGroupMessage = RDS
    type Rs DeleteDBParameterGroupMessage = DeleteDBParameterGroupResponse

    request  = post "DeleteDBParameterGroup"
    response = nullaryResponse DeleteDBParameterGroupResponse
