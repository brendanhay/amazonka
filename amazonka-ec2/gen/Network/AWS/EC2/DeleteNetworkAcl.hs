{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteNetworkAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified network ACL. You can't delete the ACL if it's
-- associated with any subnets. You can't delete the default network ACL.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteNetworkAcl.html>
module Network.AWS.EC2.DeleteNetworkAcl
    (
    -- * Request
      DeleteNetworkAcl
    -- ** Request constructor
    , deleteNetworkAcl
    -- ** Request lenses
    , dnaDryRun
    , dnaNetworkAclId

    -- * Response
    , DeleteNetworkAclResponse
    -- ** Response constructor
    , deleteNetworkAclResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteNetworkAcl = DeleteNetworkAcl
    { _dnaDryRun       :: Maybe Bool
    , _dnaNetworkAclId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteNetworkAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dnaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dnaNetworkAclId' @::@ 'Text'
--
deleteNetworkAcl :: Text -- ^ 'dnaNetworkAclId'
                 -> DeleteNetworkAcl
deleteNetworkAcl p1 = DeleteNetworkAcl
    { _dnaNetworkAclId = p1
    , _dnaDryRun       = Nothing
    }

dnaDryRun :: Lens' DeleteNetworkAcl (Maybe Bool)
dnaDryRun = lens _dnaDryRun (\s a -> s { _dnaDryRun = a })

-- | The ID of the network ACL.
dnaNetworkAclId :: Lens' DeleteNetworkAcl Text
dnaNetworkAclId = lens _dnaNetworkAclId (\s a -> s { _dnaNetworkAclId = a })

data DeleteNetworkAclResponse = DeleteNetworkAclResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteNetworkAclResponse' constructor.
deleteNetworkAclResponse :: DeleteNetworkAclResponse
deleteNetworkAclResponse = DeleteNetworkAclResponse

instance ToPath DeleteNetworkAcl where
    toPath = const "/"

instance ToQuery DeleteNetworkAcl

instance ToHeaders DeleteNetworkAcl

instance AWSRequest DeleteNetworkAcl where
    type Sv DeleteNetworkAcl = EC2
    type Rs DeleteNetworkAcl = DeleteNetworkAclResponse

    request  = post "DeleteNetworkAcl"
    response = nullResponse DeleteNetworkAclResponse
