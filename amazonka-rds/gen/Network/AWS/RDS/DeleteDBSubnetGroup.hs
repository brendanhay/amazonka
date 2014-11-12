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

-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DB subnet group.
module Network.AWS.RDS.DeleteDBSubnetGroup
    (
    -- * Request
      DeleteDBSubnetGroupMessage
    -- ** Request constructor
    , deleteDBSubnetGroupMessage
    -- ** Request lenses
    , ddbsgm1DBSubnetGroupName

    -- * Response
    , DeleteDBSubnetGroupResponse
    -- ** Response constructor
    , deleteDBSubnetGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

newtype DeleteDBSubnetGroupMessage = DeleteDBSubnetGroupMessage
    { _ddbsgm1DBSubnetGroupName :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteDBSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgm1DBSubnetGroupName' @::@ 'Text'
--
deleteDBSubnetGroupMessage :: Text -- ^ 'ddbsgm1DBSubnetGroupName'
                           -> DeleteDBSubnetGroupMessage
deleteDBSubnetGroupMessage p1 = DeleteDBSubnetGroupMessage
    { _ddbsgm1DBSubnetGroupName = p1
    }

-- | The name of the database subnet group to delete. Constraints: Must be 1
-- to 255 alphanumeric characters First character must be a letter Cannot
-- end with a hyphen or contain two consecutive hyphens.
ddbsgm1DBSubnetGroupName :: Lens' DeleteDBSubnetGroupMessage Text
ddbsgm1DBSubnetGroupName =
    lens _ddbsgm1DBSubnetGroupName
        (\s a -> s { _ddbsgm1DBSubnetGroupName = a })
instance ToQuery DeleteDBSubnetGroupMessage

instance ToPath DeleteDBSubnetGroupMessage where
    toPath = const "/"

data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDBSubnetGroupResponse' constructor.
deleteDBSubnetGroupResponse :: DeleteDBSubnetGroupResponse
deleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse

instance FromXML DeleteDBSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteDBSubnetGroupResponse"

instance AWSRequest DeleteDBSubnetGroupMessage where
    type Sv DeleteDBSubnetGroupMessage = RDS
    type Rs DeleteDBSubnetGroupMessage = DeleteDBSubnetGroupResponse

    request  = post "DeleteDBSubnetGroup"
    response = nullaryResponse DeleteDBSubnetGroupResponse
