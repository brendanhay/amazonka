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

-- Module      : Network.AWS.IAM.PutGroupPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds (or updates) a policy document associated with the specified group.
-- For information about policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html
-- Overview of Policies> in the /Using IAM/ guide. For information about
-- limits on the number of policies you can associate with a group, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html
-- Limitations on IAM Entities> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_PutGroupPolicy.html>
module Network.AWS.IAM.PutGroupPolicy
    (
    -- * Request
      PutGroupPolicy
    -- ** Request constructor
    , putGroupPolicy
    -- ** Request lenses
    , pgpGroupName
    , pgpPolicyDocument
    , pgpPolicyName

    -- * Response
    , PutGroupPolicyResponse
    -- ** Response constructor
    , putGroupPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data PutGroupPolicy = PutGroupPolicy
    { _pgpGroupName      :: Text
    , _pgpPolicyDocument :: Text
    , _pgpPolicyName     :: Text
    } deriving (Eq, Ord, Show)

-- | 'PutGroupPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pgpGroupName' @::@ 'Text'
--
-- * 'pgpPolicyDocument' @::@ 'Text'
--
-- * 'pgpPolicyName' @::@ 'Text'
--
putGroupPolicy :: Text -- ^ 'pgpGroupName'
               -> Text -- ^ 'pgpPolicyName'
               -> Text -- ^ 'pgpPolicyDocument'
               -> PutGroupPolicy
putGroupPolicy p1 p2 p3 = PutGroupPolicy
    { _pgpGroupName      = p1
    , _pgpPolicyName     = p2
    , _pgpPolicyDocument = p3
    }

-- | The name of the group to associate the policy with.
pgpGroupName :: Lens' PutGroupPolicy Text
pgpGroupName = lens _pgpGroupName (\s a -> s { _pgpGroupName = a })

-- | The policy document.
pgpPolicyDocument :: Lens' PutGroupPolicy Text
pgpPolicyDocument =
    lens _pgpPolicyDocument (\s a -> s { _pgpPolicyDocument = a })

-- | The name of the policy document.
pgpPolicyName :: Lens' PutGroupPolicy Text
pgpPolicyName = lens _pgpPolicyName (\s a -> s { _pgpPolicyName = a })

data PutGroupPolicyResponse = PutGroupPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutGroupPolicyResponse' constructor.
putGroupPolicyResponse :: PutGroupPolicyResponse
putGroupPolicyResponse = PutGroupPolicyResponse

instance ToPath PutGroupPolicy where
    toPath = const "/"

instance ToQuery PutGroupPolicy where
    toQuery PutGroupPolicy{..} = mconcat
        [ "GroupName"      =? _pgpGroupName
        , "PolicyDocument" =? _pgpPolicyDocument
        , "PolicyName"     =? _pgpPolicyName
        ]

instance ToHeaders PutGroupPolicy

instance AWSRequest PutGroupPolicy where
    type Sv PutGroupPolicy = IAM
    type Rs PutGroupPolicy = PutGroupPolicyResponse

    request  = post "PutGroupPolicy"
    response = nullResponse PutGroupPolicyResponse
