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

-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an Amazon Redshift security group. For information about managing
-- security groups, go to Amazon Redshift Cluster Security Groups in the
-- Amazon Redshift Management Guide.
module Network.AWS.Redshift.DeleteClusterSecurityGroup
    (
    -- * Request
      DeleteClusterSecurityGroupMessage
    -- ** Request constructor
    , deleteClusterSecurityGroup
    -- ** Request lenses
    , dcsgmClusterSecurityGroupName

    -- * Response
    , DeleteClusterSecurityGroupResponse
    -- ** Response constructor
    , deleteClusterSecurityGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DeleteClusterSecurityGroupMessage = DeleteClusterSecurityGroupMessage
    { _dcsgmClusterSecurityGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteClusterSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgmClusterSecurityGroupName' @::@ 'Text'
--
deleteClusterSecurityGroup :: Text -- ^ 'dcsgmClusterSecurityGroupName'
                           -> DeleteClusterSecurityGroupMessage
deleteClusterSecurityGroup p1 = DeleteClusterSecurityGroupMessage
    { _dcsgmClusterSecurityGroupName = p1
    }

-- | The name of the cluster security group to be deleted.
dcsgmClusterSecurityGroupName :: Lens' DeleteClusterSecurityGroupMessage Text
dcsgmClusterSecurityGroupName =
    lens _dcsgmClusterSecurityGroupName
        (\s a -> s { _dcsgmClusterSecurityGroupName = a })

instance ToPath DeleteClusterSecurityGroupMessage where
    toPath = const "/"

instance ToQuery DeleteClusterSecurityGroupMessage

data DeleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse

-- | 'DeleteClusterSecurityGroupResponse' constructor.
deleteClusterSecurityGroupResponse :: DeleteClusterSecurityGroupResponse
deleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse

instance AWSRequest DeleteClusterSecurityGroupMessage where
    type Sv DeleteClusterSecurityGroupMessage = Redshift
    type Rs DeleteClusterSecurityGroupMessage = DeleteClusterSecurityGroupResponse

    request  = post "DeleteClusterSecurityGroup"
    response = const (nullaryResponse DeleteClusterSecurityGroupResponse)
