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

-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DB security group.
module Network.AWS.RDS.DeleteDBSecurityGroup
    (
    -- * Request
      DeleteDBSecurityGroupMessage
    -- ** Request constructor
    , deleteDBSecurityGroupMessage
    -- ** Request lenses
    , ddbsgmDBSecurityGroupName

    -- * Response
    , DeleteDBSecurityGroupResponse
    -- ** Response constructor
    , deleteDBSecurityGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

newtype DeleteDBSecurityGroupMessage = DeleteDBSecurityGroupMessage
    { _ddbsgmDBSecurityGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteDBSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgmDBSecurityGroupName' @::@ 'Text'
--
deleteDBSecurityGroupMessage :: Text -- ^ 'ddbsgmDBSecurityGroupName'
                             -> DeleteDBSecurityGroupMessage
deleteDBSecurityGroupMessage p1 = DeleteDBSecurityGroupMessage
    { _ddbsgmDBSecurityGroupName = p1
    }

-- | The name of the DB security group to delete. Constraints: Must be 1 to
-- 255 alphanumeric characters First character must be a letter Cannot end
-- with a hyphen or contain two consecutive hyphens Must not be "Default"
-- May not contain spaces.
ddbsgmDBSecurityGroupName :: Lens' DeleteDBSecurityGroupMessage Text
ddbsgmDBSecurityGroupName =
    lens _ddbsgmDBSecurityGroupName
        (\s a -> s { _ddbsgmDBSecurityGroupName = a })
instance ToQuery DeleteDBSecurityGroupMessage

instance ToPath DeleteDBSecurityGroupMessage where
    toPath = const "/"

data DeleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDBSecurityGroupResponse' constructor.
deleteDBSecurityGroupResponse :: DeleteDBSecurityGroupResponse
deleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse
instance FromXML DeleteDBSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteDBSecurityGroupResponse"

instance AWSRequest DeleteDBSecurityGroupMessage where
    type Sv DeleteDBSecurityGroupMessage = RDS
    type Rs DeleteDBSecurityGroupMessage = DeleteDBSecurityGroupResponse

    request  = post "DeleteDBSecurityGroup"
    response = nullaryResponse DeleteDBSecurityGroupResponse
