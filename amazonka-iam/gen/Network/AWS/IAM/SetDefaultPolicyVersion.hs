{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_SetDefaultPolicyVersion.html>
module Network.AWS.IAM.SetDefaultPolicyVersion
    (
    -- * Request
      SetDefaultPolicyVersion
    -- ** Request constructor
    , setDefaultPolicyVersion
    -- ** Request lenses
    , sdpvrqPolicyARN
    , sdpvrqVersionId

    -- * Response
    , SetDefaultPolicyVersionResponse
    -- ** Response constructor
    , setDefaultPolicyVersionResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setDefaultPolicyVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdpvrqPolicyARN'
--
-- * 'sdpvrqVersionId'
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
    { _sdpvrqPolicyARN :: !Text
    , _sdpvrqVersionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetDefaultPolicyVersion' smart constructor.
setDefaultPolicyVersion :: Text -> Text -> SetDefaultPolicyVersion
setDefaultPolicyVersion pPolicyARN_ pVersionId_ =
    SetDefaultPolicyVersion'
    { _sdpvrqPolicyARN = pPolicyARN_
    , _sdpvrqVersionId = pVersionId_
    }

-- | FIXME: Undocumented member.
sdpvrqPolicyARN :: Lens' SetDefaultPolicyVersion Text
sdpvrqPolicyARN = lens _sdpvrqPolicyARN (\ s a -> s{_sdpvrqPolicyARN = a});

-- | The version of the policy to set as the default (operative) version.
--
-- For more information about managed policy versions, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies>
-- in the /Using IAM/ guide.
sdpvrqVersionId :: Lens' SetDefaultPolicyVersion Text
sdpvrqVersionId = lens _sdpvrqVersionId (\ s a -> s{_sdpvrqVersionId = a});

instance AWSRequest SetDefaultPolicyVersion where
        type Sv SetDefaultPolicyVersion = IAM
        type Rs SetDefaultPolicyVersion =
             SetDefaultPolicyVersionResponse
        request = post
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
               "PolicyArn" =: _sdpvrqPolicyARN,
               "VersionId" =: _sdpvrqVersionId]

-- | /See:/ 'setDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse =
    SetDefaultPolicyVersionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetDefaultPolicyVersionResponse' smart constructor.
setDefaultPolicyVersionResponse :: SetDefaultPolicyVersionResponse
setDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
