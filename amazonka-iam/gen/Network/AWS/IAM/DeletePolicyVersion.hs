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

-- Module      : Network.AWS.IAM.DeletePolicyVersion
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

-- | Deletes the specified version of the specified managed policy.
--
-- You cannot delete the default version of a policy using this API. To delete
-- the default version of a policy, use 'DeletePolicy'. To find out which version
-- of a policy is marked as the default version, use 'ListPolicyVersions'.
--
-- For information about versions for managed policies, refer to <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning forManaged Policies> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeletePolicyVersion.html>
module Network.AWS.IAM.DeletePolicyVersion
    (
    -- * Request
      DeletePolicyVersion
    -- ** Request constructor
    , deletePolicyVersion
    -- ** Request lenses
    , dpvPolicyArn
    , dpvVersionId

    -- * Response
    , DeletePolicyVersionResponse
    -- ** Response constructor
    , deletePolicyVersionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeletePolicyVersion = DeletePolicyVersion
    { _dpvPolicyArn :: Text
    , _dpvVersionId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeletePolicyVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpvPolicyArn' @::@ 'Text'
--
-- * 'dpvVersionId' @::@ 'Text'
--
deletePolicyVersion :: Text -- ^ 'dpvPolicyArn'
                    -> Text -- ^ 'dpvVersionId'
                    -> DeletePolicyVersion
deletePolicyVersion p1 p2 = DeletePolicyVersion
    { _dpvPolicyArn = p1
    , _dpvVersionId = p2
    }

dpvPolicyArn :: Lens' DeletePolicyVersion Text
dpvPolicyArn = lens _dpvPolicyArn (\s a -> s { _dpvPolicyArn = a })

-- | The policy version to delete.
--
-- For more information about managed policy versions, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning forManaged Policies> in the /Using IAM/ guide.
dpvVersionId :: Lens' DeletePolicyVersion Text
dpvVersionId = lens _dpvVersionId (\s a -> s { _dpvVersionId = a })

data DeletePolicyVersionResponse = DeletePolicyVersionResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeletePolicyVersionResponse' constructor.
deletePolicyVersionResponse :: DeletePolicyVersionResponse
deletePolicyVersionResponse = DeletePolicyVersionResponse

instance ToPath DeletePolicyVersion where
    toPath = const "/"

instance ToQuery DeletePolicyVersion where
    toQuery DeletePolicyVersion{..} = mconcat
        [ "PolicyArn" =? _dpvPolicyArn
        , "VersionId" =? _dpvVersionId
        ]

instance ToHeaders DeletePolicyVersion

instance AWSRequest DeletePolicyVersion where
    type Sv DeletePolicyVersion = IAM
    type Rs DeletePolicyVersion = DeletePolicyVersionResponse

    request  = post "DeletePolicyVersion"
    response = nullResponse DeletePolicyVersionResponse
