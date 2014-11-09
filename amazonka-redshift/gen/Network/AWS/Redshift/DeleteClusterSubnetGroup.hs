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

-- Module      : Network.AWS.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified cluster subnet group.
module Network.AWS.Redshift.DeleteClusterSubnetGroup
    (
    -- * Request
      DeleteClusterSubnetGroupMessage
    -- ** Request constructor
    , deleteClusterSubnetGroupMessage
    -- ** Request lenses
    , dcsgmClusterSubnetGroupName

    -- * Response
    , DeleteClusterSubnetGroupResponse
    -- ** Response constructor
    , deleteClusterSubnetGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DeleteClusterSubnetGroupMessage = DeleteClusterSubnetGroupMessage
    { _dcsgmClusterSubnetGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteClusterSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgmClusterSubnetGroupName' @::@ 'Text'
--
deleteClusterSubnetGroupMessage :: Text -- ^ 'dcsgmClusterSubnetGroupName'
                                -> DeleteClusterSubnetGroupMessage
deleteClusterSubnetGroupMessage p1 = DeleteClusterSubnetGroupMessage
    { _dcsgmClusterSubnetGroupName = p1
    }

-- | The name of the cluster subnet group name to be deleted.
dcsgmClusterSubnetGroupName :: Lens' DeleteClusterSubnetGroupMessage Text
dcsgmClusterSubnetGroupName =
    lens _dcsgmClusterSubnetGroupName
        (\s a -> s { _dcsgmClusterSubnetGroupName = a })

instance ToPath DeleteClusterSubnetGroupMessage where
    toPath = const "/"

instance ToQuery DeleteClusterSubnetGroupMessage

data DeleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse

-- | 'DeleteClusterSubnetGroupResponse' constructor.
deleteClusterSubnetGroupResponse :: DeleteClusterSubnetGroupResponse
deleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse

instance AWSRequest DeleteClusterSubnetGroupMessage where
    type Sv DeleteClusterSubnetGroupMessage = Redshift
    type Rs DeleteClusterSubnetGroupMessage = DeleteClusterSubnetGroupResponse

    request  = post "DeleteClusterSubnetGroup"
    response = const (nullaryResponse DeleteClusterSubnetGroupResponse)
