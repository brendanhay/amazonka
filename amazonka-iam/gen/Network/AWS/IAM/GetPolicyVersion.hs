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

-- Module      : Network.AWS.IAM.GetPolicyVersion
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

-- | Retrieves information about the specified version of the specified managed
-- policy, including the policy document.
--
-- To list the available versions for a policy, use 'ListPolicyVersions'.
--
-- This API retrieves information about managed policies. To retrieve
-- information about an inline policy that is embedded in a user, group, or
-- role, use the 'GetUserPolicy', 'GetGroupPolicy', or 'GetRolePolicy' API.
--
-- For more information about the types of policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policiesand Inline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetPolicyVersion.html>
module Network.AWS.IAM.GetPolicyVersion
    (
    -- * Request
      GetPolicyVersion
    -- ** Request constructor
    , getPolicyVersion
    -- ** Request lenses
    , gpvPolicyArn
    , gpvVersionId

    -- * Response
    , GetPolicyVersionResponse
    -- ** Response constructor
    , getPolicyVersionResponse
    -- ** Response lenses
    , gpvrPolicyVersion
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data GetPolicyVersion = GetPolicyVersion
    { _gpvPolicyArn :: Text
    , _gpvVersionId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetPolicyVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpvPolicyArn' @::@ 'Text'
--
-- * 'gpvVersionId' @::@ 'Text'
--
getPolicyVersion :: Text -- ^ 'gpvPolicyArn'
                 -> Text -- ^ 'gpvVersionId'
                 -> GetPolicyVersion
getPolicyVersion p1 p2 = GetPolicyVersion
    { _gpvPolicyArn = p1
    , _gpvVersionId = p2
    }

gpvPolicyArn :: Lens' GetPolicyVersion Text
gpvPolicyArn = lens _gpvPolicyArn (\s a -> s { _gpvPolicyArn = a })

-- | Identifies the policy version to retrieve.
gpvVersionId :: Lens' GetPolicyVersion Text
gpvVersionId = lens _gpvVersionId (\s a -> s { _gpvVersionId = a })

newtype GetPolicyVersionResponse = GetPolicyVersionResponse
    { _gpvrPolicyVersion :: Maybe PolicyVersion
    } deriving (Eq, Read, Show)

-- | 'GetPolicyVersionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpvrPolicyVersion' @::@ 'Maybe' 'PolicyVersion'
--
getPolicyVersionResponse :: GetPolicyVersionResponse
getPolicyVersionResponse = GetPolicyVersionResponse
    { _gpvrPolicyVersion = Nothing
    }

-- | Information about the policy version.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning forManaged Policies> in the /Using IAM/ guide.
gpvrPolicyVersion :: Lens' GetPolicyVersionResponse (Maybe PolicyVersion)
gpvrPolicyVersion =
    lens _gpvrPolicyVersion (\s a -> s { _gpvrPolicyVersion = a })

instance ToPath GetPolicyVersion where
    toPath = const "/"

instance ToQuery GetPolicyVersion where
    toQuery GetPolicyVersion{..} = mconcat
        [ "PolicyArn" =? _gpvPolicyArn
        , "VersionId" =? _gpvVersionId
        ]

instance ToHeaders GetPolicyVersion

instance AWSRequest GetPolicyVersion where
    type Sv GetPolicyVersion = IAM
    type Rs GetPolicyVersion = GetPolicyVersionResponse

    request  = post "GetPolicyVersion"
    response = xmlResponse

instance FromXML GetPolicyVersionResponse where
    parseXML = withElement "GetPolicyVersionResult" $ \x -> GetPolicyVersionResponse
        <$> x .@? "PolicyVersion"
