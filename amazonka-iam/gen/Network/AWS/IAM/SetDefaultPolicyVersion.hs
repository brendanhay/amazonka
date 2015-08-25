{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy\'s
-- default (operative) version.
--
-- This action affects all users, groups, and roles that the policy is
-- attached to. To list the users, groups, and roles that the policy is
-- attached to, use the ListEntitiesForPolicy API.
--
-- For information about managed policies, refer to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_SetDefaultPolicyVersion.html AWS API Reference> for SetDefaultPolicyVersion.
module Network.AWS.IAM.SetDefaultPolicyVersion
    (
    -- * Creating a Request
      setDefaultPolicyVersion
    , SetDefaultPolicyVersion
    -- * Request Lenses
    , sdpvPolicyARN
    , sdpvVersionId

    -- * Destructuring the Response
    , setDefaultPolicyVersionResponse
    , SetDefaultPolicyVersionResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
    { _sdpvPolicyARN :: !Text
    , _sdpvVersionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetDefaultPolicyVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdpvPolicyARN'
--
-- * 'sdpvVersionId'
setDefaultPolicyVersion
    :: Text -- ^ 'sdpvPolicyARN'
    -> Text -- ^ 'sdpvVersionId'
    -> SetDefaultPolicyVersion
setDefaultPolicyVersion pPolicyARN_ pVersionId_ =
    SetDefaultPolicyVersion'
    { _sdpvPolicyARN = pPolicyARN_
    , _sdpvVersionId = pVersionId_
    }

-- | Undocumented member.
sdpvPolicyARN :: Lens' SetDefaultPolicyVersion Text
sdpvPolicyARN = lens _sdpvPolicyARN (\ s a -> s{_sdpvPolicyARN = a});

-- | The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
sdpvVersionId :: Lens' SetDefaultPolicyVersion Text
sdpvVersionId = lens _sdpvVersionId (\ s a -> s{_sdpvVersionId = a});

instance AWSRequest SetDefaultPolicyVersion where
        type Rs SetDefaultPolicyVersion =
             SetDefaultPolicyVersionResponse
        request = postQuery iAM
        response
          = receiveNull SetDefaultPolicyVersionResponse'

instance ToHeaders SetDefaultPolicyVersion where
        toHeaders = const mempty

instance ToPath SetDefaultPolicyVersion where
        toPath = const "/"

instance ToQuery SetDefaultPolicyVersion where
        toQuery SetDefaultPolicyVersion'{..}
          = mconcat
              ["Action" =:
                 ("SetDefaultPolicyVersion" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "PolicyArn" =: _sdpvPolicyARN,
               "VersionId" =: _sdpvVersionId]

-- | /See:/ 'setDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse =
    SetDefaultPolicyVersionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SetDefaultPolicyVersionResponse' with the minimum fields required to make a request.
--
setDefaultPolicyVersionResponse
    :: SetDefaultPolicyVersionResponse
setDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
