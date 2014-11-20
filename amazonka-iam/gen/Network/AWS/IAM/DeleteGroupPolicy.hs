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

-- Module      : Network.AWS.IAM.DeleteGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy that is associated with the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteGroupPolicy.html>
module Network.AWS.IAM.DeleteGroupPolicy
    (
    -- * Request
      DeleteGroupPolicy
    -- ** Request constructor
    , deleteGroupPolicy
    -- ** Request lenses
    , dgpGroupName
    , dgpPolicyName

    -- * Response
    , DeleteGroupPolicyResponse
    -- ** Response constructor
    , deleteGroupPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeleteGroupPolicy = DeleteGroupPolicy
    { _dgpGroupName  :: Text
    , _dgpPolicyName :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteGroupPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dgpGroupName' @::@ 'Text'
--
-- * 'dgpPolicyName' @::@ 'Text'
--
deleteGroupPolicy :: Text -- ^ 'dgpGroupName'
                  -> Text -- ^ 'dgpPolicyName'
                  -> DeleteGroupPolicy
deleteGroupPolicy p1 p2 = DeleteGroupPolicy
    { _dgpGroupName  = p1
    , _dgpPolicyName = p2
    }

-- | The name of the group the policy is associated with.
dgpGroupName :: Lens' DeleteGroupPolicy Text
dgpGroupName = lens _dgpGroupName (\s a -> s { _dgpGroupName = a })

-- | The name of the policy document to delete.
dgpPolicyName :: Lens' DeleteGroupPolicy Text
dgpPolicyName = lens _dgpPolicyName (\s a -> s { _dgpPolicyName = a })

data DeleteGroupPolicyResponse = DeleteGroupPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteGroupPolicyResponse' constructor.
deleteGroupPolicyResponse :: DeleteGroupPolicyResponse
deleteGroupPolicyResponse = DeleteGroupPolicyResponse

instance ToPath DeleteGroupPolicy where
    toPath = const "/"

instance ToQuery DeleteGroupPolicy where
    toQuery DeleteGroupPolicy{..} = mconcat
        [ "GroupName"  =? _dgpGroupName
        , "PolicyName" =? _dgpPolicyName
        ]

instance ToHeaders DeleteGroupPolicy

query

instance AWSRequest DeleteGroupPolicy where
    type Sv DeleteGroupPolicy = IAM
    type Rs DeleteGroupPolicy = DeleteGroupPolicyResponse

    request  = post "DeleteGroupPolicy"
    response = nullResponse DeleteGroupPolicyResponse
