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

-- Module      : Network.AWS.IAM.DeleteUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified policy associated with the specified user.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteUserPolicy.html>
module Network.AWS.IAM.DeleteUserPolicy
    (
    -- * Request
      DeleteUserPolicy
    -- ** Request constructor
    , deleteUserPolicy
    -- ** Request lenses
    , dupPolicyName
    , dupUserName

    -- * Response
    , DeleteUserPolicyResponse
    -- ** Response constructor
    , deleteUserPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeleteUserPolicy = DeleteUserPolicy
    { _dupPolicyName :: Text
    , _dupUserName   :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteUserPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dupPolicyName' @::@ 'Text'
--
-- * 'dupUserName' @::@ 'Text'
--
deleteUserPolicy :: Text -- ^ 'dupUserName'
                 -> Text -- ^ 'dupPolicyName'
                 -> DeleteUserPolicy
deleteUserPolicy p1 p2 = DeleteUserPolicy
    { _dupUserName   = p1
    , _dupPolicyName = p2
    }

-- | The name of the policy document to delete.
dupPolicyName :: Lens' DeleteUserPolicy Text
dupPolicyName = lens _dupPolicyName (\s a -> s { _dupPolicyName = a })

-- | The name of the user the policy is associated with.
dupUserName :: Lens' DeleteUserPolicy Text
dupUserName = lens _dupUserName (\s a -> s { _dupUserName = a })

data DeleteUserPolicyResponse = DeleteUserPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteUserPolicyResponse' constructor.
deleteUserPolicyResponse :: DeleteUserPolicyResponse
deleteUserPolicyResponse = DeleteUserPolicyResponse

instance ToPath DeleteUserPolicy where
    toPath = const "/"

instance ToQuery DeleteUserPolicy where
    toQuery DeleteUserPolicy{..} = mconcat
        [ "PolicyName" =? _dupPolicyName
        , "UserName"   =? _dupUserName
        ]

instance ToHeaders DeleteUserPolicy

query

instance AWSRequest DeleteUserPolicy where
    type Sv DeleteUserPolicy = IAM
    type Rs DeleteUserPolicy = DeleteUserPolicyResponse

    request  = post "DeleteUserPolicy"
    response = nullResponse DeleteUserPolicyResponse
