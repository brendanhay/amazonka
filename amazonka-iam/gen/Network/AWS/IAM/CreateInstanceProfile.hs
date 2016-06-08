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
-- Module      : Network.AWS.IAM.CreateInstanceProfile
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new instance profile. For information about instance profiles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- For information about the number of instance profiles you can create, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/.
module Network.AWS.IAM.CreateInstanceProfile
    (
    -- * Creating a Request
      createInstanceProfile
    , CreateInstanceProfile
    -- * Request Lenses
    , cipPath
    , cipInstanceProfileName

    -- * Destructuring the Response
    , createInstanceProfileResponse
    , CreateInstanceProfileResponse
    -- * Response Lenses
    , ciprsResponseStatus
    , ciprsInstanceProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
    { _cipPath                :: !(Maybe Text)
    , _cipInstanceProfileName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cipPath'
--
-- * 'cipInstanceProfileName'
createInstanceProfile
    :: Text -- ^ 'cipInstanceProfileName'
    -> CreateInstanceProfile
createInstanceProfile pInstanceProfileName_ =
    CreateInstanceProfile'
    { _cipPath = Nothing
    , _cipInstanceProfileName = pInstanceProfileName_
    }

-- | The path to the instance profile. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a slash (\/).
cipPath :: Lens' CreateInstanceProfile (Maybe Text)
cipPath = lens _cipPath (\ s a -> s{_cipPath = a});

-- | The name of the instance profile to create.
cipInstanceProfileName :: Lens' CreateInstanceProfile Text
cipInstanceProfileName = lens _cipInstanceProfileName (\ s a -> s{_cipInstanceProfileName = a});

instance AWSRequest CreateInstanceProfile where
        type Rs CreateInstanceProfile =
             CreateInstanceProfileResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreateInstanceProfileResult"
              (\ s h x ->
                 CreateInstanceProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "InstanceProfile"))

instance Hashable CreateInstanceProfile

instance NFData CreateInstanceProfile

instance ToHeaders CreateInstanceProfile where
        toHeaders = const mempty

instance ToPath CreateInstanceProfile where
        toPath = const "/"

instance ToQuery CreateInstanceProfile where
        toQuery CreateInstanceProfile'{..}
          = mconcat
              ["Action" =: ("CreateInstanceProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cipPath,
               "InstanceProfileName" =: _cipInstanceProfileName]

-- | Contains the response to a successful < CreateInstanceProfile> request.
--
-- /See:/ 'createInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
    { _ciprsResponseStatus  :: !Int
    , _ciprsInstanceProfile :: !InstanceProfile
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateInstanceProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciprsResponseStatus'
--
-- * 'ciprsInstanceProfile'
createInstanceProfileResponse
    :: Int -- ^ 'ciprsResponseStatus'
    -> InstanceProfile -- ^ 'ciprsInstanceProfile'
    -> CreateInstanceProfileResponse
createInstanceProfileResponse pResponseStatus_ pInstanceProfile_ =
    CreateInstanceProfileResponse'
    { _ciprsResponseStatus = pResponseStatus_
    , _ciprsInstanceProfile = pInstanceProfile_
    }

-- | The response status code.
ciprsResponseStatus :: Lens' CreateInstanceProfileResponse Int
ciprsResponseStatus = lens _ciprsResponseStatus (\ s a -> s{_ciprsResponseStatus = a});

-- | Information about the instance profile.
ciprsInstanceProfile :: Lens' CreateInstanceProfileResponse InstanceProfile
ciprsInstanceProfile = lens _ciprsInstanceProfile (\ s a -> s{_ciprsInstanceProfile = a});

instance NFData CreateInstanceProfileResponse
