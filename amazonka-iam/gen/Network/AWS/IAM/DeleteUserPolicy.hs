{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- https://iam.amazonaws.com/ ?Action=DeleteUserPolicy &UserName=Bob
-- &PolicyName=AllAccessPolicy &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM
    (
    -- * Request
      DeleteUserPolicy
    -- ** Request constructor
    , mkDeleteUserPolicy
    -- ** Request lenses
    , dupUserName
    , dupPolicyName

    -- * Response
    , DeleteUserPolicyResponse
    -- ** Response constructor
    , mkDeleteUserPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data DeleteUserPolicy = DeleteUserPolicy
    { _dupUserName :: !Text
    , _dupPolicyName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteUserPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @PolicyName ::@ @Text@
--
mkDeleteUserPolicy :: Text -- ^ 'dupUserName'
                   -> Text -- ^ 'dupPolicyName'
                   -> DeleteUserPolicy
mkDeleteUserPolicy p1 p2 = DeleteUserPolicy
    { _dupUserName = p1
    , _dupPolicyName = p2
    }

-- | Name of the user the policy is associated with.
dupUserName :: Lens' DeleteUserPolicy Text
dupUserName = lens _dupUserName (\s a -> s { _dupUserName = a })

-- | Name of the policy document to delete.
dupPolicyName :: Lens' DeleteUserPolicy Text
dupPolicyName = lens _dupPolicyName (\s a -> s { _dupPolicyName = a })

instance ToQuery DeleteUserPolicy where
    toQuery = genericQuery def

data DeleteUserPolicyResponse = DeleteUserPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteUserPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteUserPolicyResponse :: DeleteUserPolicyResponse
mkDeleteUserPolicyResponse = DeleteUserPolicyResponse

instance AWSRequest DeleteUserPolicy where
    type Sv DeleteUserPolicy = IAM
    type Rs DeleteUserPolicy = DeleteUserPolicyResponse

    request = post "DeleteUserPolicy"
    response _ = nullaryResponse DeleteUserPolicyResponse
