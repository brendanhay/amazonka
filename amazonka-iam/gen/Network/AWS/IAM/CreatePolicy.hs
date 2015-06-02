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

-- Module      : Network.AWS.IAM.CreatePolicy
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

-- | Creates a new managed policy for your AWS account.
--
-- This operation creates a policy version with a version identifier of 'v1' and
-- sets v1 as the policy's default version. For more information about policy
-- versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /Using IAM/ guide.
--
-- For more information about managed policies in general, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html ManagedPolicies and Inline Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html>
module Network.AWS.IAM.CreatePolicy
    (
    -- * Request
      CreatePolicy
    -- ** Request constructor
    , createPolicy
    -- ** Request lenses
    , cpDescription
    , cpPath
    , cpPolicyDocument
    , cpPolicyName

    -- * Response
    , CreatePolicyResponse
    -- ** Response constructor
    , createPolicyResponse
    -- ** Response lenses
    , cprPolicy
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreatePolicy = CreatePolicy
    { _cpDescription    :: Maybe Text
    , _cpPath           :: Maybe Text
    , _cpPolicyDocument :: Text
    , _cpPolicyName     :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreatePolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpDescription' @::@ 'Maybe' 'Text'
--
-- * 'cpPath' @::@ 'Maybe' 'Text'
--
-- * 'cpPolicyDocument' @::@ 'Text'
--
-- * 'cpPolicyName' @::@ 'Text'
--
createPolicy :: Text -- ^ 'cpPolicyName'
             -> Text -- ^ 'cpPolicyDocument'
             -> CreatePolicy
createPolicy p1 p2 = CreatePolicy
    { _cpPolicyName     = p1
    , _cpPolicyDocument = p2
    , _cpPath           = Nothing
    , _cpDescription    = Nothing
    }

-- | A friendly description of the policy.
--
-- Typically used to store information about the permissions defined in the
-- policy. For example, "Grants access to production DynamoDB tables."
--
-- The policy description is immutable. After a value is assigned, it cannot be
-- changed.
cpDescription :: Lens' CreatePolicy (Maybe Text)
cpDescription = lens _cpDescription (\s a -> s { _cpDescription = a })

-- | The path for the policy.
--
-- For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/
-- guide.
--
-- This parameter is optional. If it is not included, it defaults to a slash
-- (/).
cpPath :: Lens' CreatePolicy (Maybe Text)
cpPath = lens _cpPath (\s a -> s { _cpPath = a })

-- | The policy document.
cpPolicyDocument :: Lens' CreatePolicy Text
cpPolicyDocument = lens _cpPolicyDocument (\s a -> s { _cpPolicyDocument = a })

-- | The name of the policy document.
cpPolicyName :: Lens' CreatePolicy Text
cpPolicyName = lens _cpPolicyName (\s a -> s { _cpPolicyName = a })

newtype CreatePolicyResponse = CreatePolicyResponse
    { _cprPolicy :: Maybe Policy
    } deriving (Eq, Read, Show)

-- | 'CreatePolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprPolicy' @::@ 'Maybe' 'Policy'
--
createPolicyResponse :: CreatePolicyResponse
createPolicyResponse = CreatePolicyResponse
    { _cprPolicy = Nothing
    }

-- | Information about the policy.
cprPolicy :: Lens' CreatePolicyResponse (Maybe Policy)
cprPolicy = lens _cprPolicy (\s a -> s { _cprPolicy = a })

instance ToPath CreatePolicy where
    toPath = const "/"

instance ToQuery CreatePolicy where
    toQuery CreatePolicy{..} = mconcat
        [ "Description"    =? _cpDescription
        , "Path"           =? _cpPath
        , "PolicyDocument" =? _cpPolicyDocument
        , "PolicyName"     =? _cpPolicyName
        ]

instance ToHeaders CreatePolicy

instance AWSRequest CreatePolicy where
    type Sv CreatePolicy = IAM
    type Rs CreatePolicy = CreatePolicyResponse

    request  = post "CreatePolicy"
    response = xmlResponse

instance FromXML CreatePolicyResponse where
    parseXML = withElement "CreatePolicyResult" $ \x -> CreatePolicyResponse
        <$> x .@? "Policy"
