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

-- Module      : Network.AWS.IAM.SetDefaultPolicyVersion
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

-- | Sets the specified version of the specified policy as the policy's default
-- (operative) version.
--
-- This action affects all users, groups, and roles that the policy is attached
-- to. To list the users, groups, and roles that the policy is attached to, use
-- the 'ListEntitiesForPolicy' API.
--
-- For information about managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and InlinePolicies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_SetDefaultPolicyVersion.html>
module Network.AWS.IAM.SetDefaultPolicyVersion
    (
    -- * Request
      SetDefaultPolicyVersion
    -- ** Request constructor
    , setDefaultPolicyVersion
    -- ** Request lenses
    , sdpvPolicyArn
    , sdpvVersionId

    -- * Response
    , SetDefaultPolicyVersionResponse
    -- ** Response constructor
    , setDefaultPolicyVersionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data SetDefaultPolicyVersion = SetDefaultPolicyVersion
    { _sdpvPolicyArn :: Text
    , _sdpvVersionId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SetDefaultPolicyVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdpvPolicyArn' @::@ 'Text'
--
-- * 'sdpvVersionId' @::@ 'Text'
--
setDefaultPolicyVersion :: Text -- ^ 'sdpvPolicyArn'
                        -> Text -- ^ 'sdpvVersionId'
                        -> SetDefaultPolicyVersion
setDefaultPolicyVersion p1 p2 = SetDefaultPolicyVersion
    { _sdpvPolicyArn = p1
    , _sdpvVersionId = p2
    }

sdpvPolicyArn :: Lens' SetDefaultPolicyVersion Text
sdpvPolicyArn = lens _sdpvPolicyArn (\s a -> s { _sdpvPolicyArn = a })

-- | The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning forManaged Policies> in the /Using IAM/ guide.
sdpvVersionId :: Lens' SetDefaultPolicyVersion Text
sdpvVersionId = lens _sdpvVersionId (\s a -> s { _sdpvVersionId = a })

data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetDefaultPolicyVersionResponse' constructor.
setDefaultPolicyVersionResponse :: SetDefaultPolicyVersionResponse
setDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse

instance ToPath SetDefaultPolicyVersion where
    toPath = const "/"

instance ToQuery SetDefaultPolicyVersion where
    toQuery SetDefaultPolicyVersion{..} = mconcat
        [ "PolicyArn" =? _sdpvPolicyArn
        , "VersionId" =? _sdpvVersionId
        ]

instance ToHeaders SetDefaultPolicyVersion

instance AWSRequest SetDefaultPolicyVersion where
    type Sv SetDefaultPolicyVersion = IAM
    type Rs SetDefaultPolicyVersion = SetDefaultPolicyVersionResponse

    request  = post "SetDefaultPolicyVersion"
    response = nullResponse SetDefaultPolicyVersionResponse
