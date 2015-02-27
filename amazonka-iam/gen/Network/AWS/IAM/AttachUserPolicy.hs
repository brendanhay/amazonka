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

-- Module      : Network.AWS.IAM.AttachUserPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Attaches the specified managed policy to the specified user.
--
-- You use this API to attach a managed policy to a user. To embed an inline
-- policy in a user, use 'PutUserPolicy'.
--
-- For more information about policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and InlinePolicies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AttachUserPolicy.html>
module Network.AWS.IAM.AttachUserPolicy
    (
    -- * Request
      AttachUserPolicy
    -- ** Request constructor
    , attachUserPolicy
    -- ** Request lenses
    , aupPolicyArn
    , aupUserName

    -- * Response
    , AttachUserPolicyResponse
    -- ** Response constructor
    , attachUserPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data AttachUserPolicy = AttachUserPolicy
    { _aupPolicyArn :: Text
    , _aupUserName  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachUserPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aupPolicyArn' @::@ 'Text'
--
-- * 'aupUserName' @::@ 'Text'
--
attachUserPolicy :: Text -- ^ 'aupUserName'
                 -> Text -- ^ 'aupPolicyArn'
                 -> AttachUserPolicy
attachUserPolicy p1 p2 = AttachUserPolicy
    { _aupUserName  = p1
    , _aupPolicyArn = p2
    }

aupPolicyArn :: Lens' AttachUserPolicy Text
aupPolicyArn = lens _aupPolicyArn (\s a -> s { _aupPolicyArn = a })

-- | The name (friendly name, not ARN) of the user to attach the policy to.
aupUserName :: Lens' AttachUserPolicy Text
aupUserName = lens _aupUserName (\s a -> s { _aupUserName = a })

data AttachUserPolicyResponse = AttachUserPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AttachUserPolicyResponse' constructor.
attachUserPolicyResponse :: AttachUserPolicyResponse
attachUserPolicyResponse = AttachUserPolicyResponse

instance ToPath AttachUserPolicy where
    toPath = const "/"

instance ToQuery AttachUserPolicy where
    toQuery AttachUserPolicy{..} = mconcat
        [ "PolicyArn" =? _aupPolicyArn
        , "UserName"  =? _aupUserName
        ]

instance ToHeaders AttachUserPolicy

instance AWSRequest AttachUserPolicy where
    type Sv AttachUserPolicy = IAM
    type Rs AttachUserPolicy = AttachUserPolicyResponse

    request  = post "AttachUserPolicy"
    response = nullResponse AttachUserPolicyResponse
