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

-- Module      : Network.AWS.IAM.CreatePolicyVersion
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

-- | Creates a new version of the specified managed policy. To update a managed
-- policy, you create a new policy version. A managed policy can have up to five
-- versions. If the policy has five versions, you must delete an existing
-- version using 'DeletePolicyVersion' before you create a new version.
--
-- Optionally, you can set the new version as the policy's default version. The
-- default version is the operative version; that is, the version that is in
-- effect for the IAM users, groups, and roles that the policy is attached to.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning forManaged Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicyVersion.html>
module Network.AWS.IAM.CreatePolicyVersion
    (
    -- * Request
      CreatePolicyVersion
    -- ** Request constructor
    , createPolicyVersion
    -- ** Request lenses
    , cpvPolicyArn
    , cpvPolicyDocument
    , cpvSetAsDefault

    -- * Response
    , CreatePolicyVersionResponse
    -- ** Response constructor
    , createPolicyVersionResponse
    -- ** Response lenses
    , cpvrPolicyVersion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreatePolicyVersion = CreatePolicyVersion
    { _cpvPolicyArn      :: Text
    , _cpvPolicyDocument :: Text
    , _cpvSetAsDefault   :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'CreatePolicyVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvPolicyArn' @::@ 'Text'
--
-- * 'cpvPolicyDocument' @::@ 'Text'
--
-- * 'cpvSetAsDefault' @::@ 'Maybe' 'Bool'
--
createPolicyVersion :: Text -- ^ 'cpvPolicyArn'
                    -> Text -- ^ 'cpvPolicyDocument'
                    -> CreatePolicyVersion
createPolicyVersion p1 p2 = CreatePolicyVersion
    { _cpvPolicyArn      = p1
    , _cpvPolicyDocument = p2
    , _cpvSetAsDefault   = Nothing
    }

cpvPolicyArn :: Lens' CreatePolicyVersion Text
cpvPolicyArn = lens _cpvPolicyArn (\s a -> s { _cpvPolicyArn = a })

-- | The policy document.
--
-- The policy must be URL-encoded according to <http://www.faqs.org/rfcs/rfc3986.html RFC 3986>.
cpvPolicyDocument :: Lens' CreatePolicyVersion Text
cpvPolicyDocument =
    lens _cpvPolicyDocument (\s a -> s { _cpvPolicyDocument = a })

-- | Specifies whether to set this version as the policy's default version.
--
-- When this parameter is 'true', the new policy version becomes the operative
-- version; that is, the version that is in effect for the IAM users, groups,
-- and roles that the policy is attached to.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning forManaged Policies> in the /Using IAM/ guide.
cpvSetAsDefault :: Lens' CreatePolicyVersion (Maybe Bool)
cpvSetAsDefault = lens _cpvSetAsDefault (\s a -> s { _cpvSetAsDefault = a })

newtype CreatePolicyVersionResponse = CreatePolicyVersionResponse
    { _cpvrPolicyVersion :: Maybe PolicyVersion
    } deriving (Eq, Read, Show)

-- | 'CreatePolicyVersionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvrPolicyVersion' @::@ 'Maybe' 'PolicyVersion'
--
createPolicyVersionResponse :: CreatePolicyVersionResponse
createPolicyVersionResponse = CreatePolicyVersionResponse
    { _cpvrPolicyVersion = Nothing
    }

-- | Information about the policy version.
cpvrPolicyVersion :: Lens' CreatePolicyVersionResponse (Maybe PolicyVersion)
cpvrPolicyVersion =
    lens _cpvrPolicyVersion (\s a -> s { _cpvrPolicyVersion = a })

instance ToPath CreatePolicyVersion where
    toPath = const "/"

instance ToQuery CreatePolicyVersion where
    toQuery CreatePolicyVersion{..} = mconcat
        [ "PolicyArn"      =? _cpvPolicyArn
        , "PolicyDocument" =? _cpvPolicyDocument
        , "SetAsDefault"   =? _cpvSetAsDefault
        ]

instance ToHeaders CreatePolicyVersion

instance AWSRequest CreatePolicyVersion where
    type Sv CreatePolicyVersion = IAM
    type Rs CreatePolicyVersion = CreatePolicyVersionResponse

    request  = post "CreatePolicyVersion"
    response = xmlResponse

instance FromXML CreatePolicyVersionResponse where
    parseXML = withElement "CreatePolicyVersionResult" $ \x -> CreatePolicyVersionResponse
        <$> x .@? "PolicyVersion"
