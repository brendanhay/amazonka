{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.SetVisibleToAllUsers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets whether all AWS Identity and Access Management (IAM) users under your
-- account can access the specified job flows. This action works on running
-- job flows. You can also set the visibility of a job flow when you launch it
-- using the VisibleToAllUsers parameter of RunJobFlow. The
-- SetVisibleToAllUsers action can be called only by an IAM user who created
-- the job flow or the AWS account that owns the job flow.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_SetVisibleToAllUsers.html>
module Network.AWS.EMR.SetVisibleToAllUsers
    (
    -- * Request
      SetVisibleToAllUsers
    -- ** Request constructor
    , setVisibleToAllUsers
    -- ** Request lenses
    , svtauJobFlowIds
    , svtauVisibleToAllUsers

    -- * Response
    , SetVisibleToAllUsersResponse
    -- ** Response constructor
    , setVisibleToAllUsersResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data SetVisibleToAllUsers = SetVisibleToAllUsers
    { _svtauJobFlowIds        :: [Text]
    , _svtauVisibleToAllUsers :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetVisibleToAllUsers' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'svtauJobFlowIds' @::@ ['Text']
--
-- * 'svtauVisibleToAllUsers' @::@ 'Bool'
--
setVisibleToAllUsers :: Bool -- ^ 'svtauVisibleToAllUsers'
                     -> SetVisibleToAllUsers
setVisibleToAllUsers p1 = SetVisibleToAllUsers
    { _svtauVisibleToAllUsers = p1
    , _svtauJobFlowIds        = mempty
    }

-- | Identifiers of the job flows to receive the new visibility setting.
svtauJobFlowIds :: Lens' SetVisibleToAllUsers [Text]
svtauJobFlowIds = lens _svtauJobFlowIds (\s a -> s { _svtauJobFlowIds = a })

-- | Whether the specified job flows are visible to all IAM users of the AWS
-- account associated with the job flow. If this value is set to True, all
-- IAM users of that AWS account can view and, if they have the proper IAM
-- policy permissions set, manage the job flows. If it is set to False, only
-- the IAM user that created a job flow can view and manage it.
svtauVisibleToAllUsers :: Lens' SetVisibleToAllUsers Bool
svtauVisibleToAllUsers =
    lens _svtauVisibleToAllUsers (\s a -> s { _svtauVisibleToAllUsers = a })

data SetVisibleToAllUsersResponse = SetVisibleToAllUsersResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetVisibleToAllUsersResponse' constructor.
setVisibleToAllUsersResponse :: SetVisibleToAllUsersResponse
setVisibleToAllUsersResponse = SetVisibleToAllUsersResponse

instance AWSRequest SetVisibleToAllUsers where
    type Sv SetVisibleToAllUsers = EMR
    type Rs SetVisibleToAllUsers = SetVisibleToAllUsersResponse

    request  = post
    response = nullResponse SetVisibleToAllUsersResponse

instance ToPath SetVisibleToAllUsers where
    toPath = const "/"

instance ToHeaders SetVisibleToAllUsers

instance ToQuery SetVisibleToAllUsers where
    toQuery = const mempty

instance ToJSON SetVisibleToAllUsers where
    toJSON = genericToJSON jsonOptions
