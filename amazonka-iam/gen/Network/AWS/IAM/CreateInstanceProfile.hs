{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateInstanceProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new instance profile. For information about instance profiles,
-- go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- For information about the number of instance profiles you can create,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateInstanceProfile.html>
module Network.AWS.IAM.CreateInstanceProfile
    (
    -- * Request
      CreateInstanceProfile
    -- ** Request constructor
    , createInstanceProfile
    -- ** Request lenses
    , cipPath
    , cipInstanceProfileName

    -- * Response
    , CreateInstanceProfileResponse
    -- ** Response constructor
    , createInstanceProfileResponse
    -- ** Response lenses
    , ciprsStatus
    , ciprsInstanceProfile
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cipPath'
--
-- * 'cipInstanceProfileName'
data CreateInstanceProfile = CreateInstanceProfile'
    { _cipPath                :: !(Maybe Text)
    , _cipInstanceProfileName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInstanceProfile' smart constructor.
createInstanceProfile :: Text -> CreateInstanceProfile
createInstanceProfile pInstanceProfileName_ =
    CreateInstanceProfile'
    { _cipPath = Nothing
    , _cipInstanceProfileName = pInstanceProfileName_
    }

-- | The path to the instance profile. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
cipPath :: Lens' CreateInstanceProfile (Maybe Text)
cipPath = lens _cipPath (\ s a -> s{_cipPath = a});

-- | The name of the instance profile to create.
cipInstanceProfileName :: Lens' CreateInstanceProfile Text
cipInstanceProfileName = lens _cipInstanceProfileName (\ s a -> s{_cipInstanceProfileName = a});

instance AWSRequest CreateInstanceProfile where
        type Sv CreateInstanceProfile = IAM
        type Rs CreateInstanceProfile =
             CreateInstanceProfileResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreateInstanceProfileResult"
              (\ s h x ->
                 CreateInstanceProfileResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "InstanceProfile"))

instance ToHeaders CreateInstanceProfile where
        toHeaders = const mempty

instance ToPath CreateInstanceProfile where
        toPath = const mempty

instance ToQuery CreateInstanceProfile where
        toQuery CreateInstanceProfile'{..}
          = mconcat
              ["Action" =: ("CreateInstanceProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cipPath,
               "InstanceProfileName" =: _cipInstanceProfileName]

-- | Contains the response to a successful CreateInstanceProfile request.
--
-- /See:/ 'createInstanceProfileResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciprsStatus'
--
-- * 'ciprsInstanceProfile'
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
    { _ciprsStatus          :: !Int
    , _ciprsInstanceProfile :: !InstanceProfile
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateInstanceProfileResponse' smart constructor.
createInstanceProfileResponse :: Int -> InstanceProfile -> CreateInstanceProfileResponse
createInstanceProfileResponse pStatus_ pInstanceProfile_ =
    CreateInstanceProfileResponse'
    { _ciprsStatus = pStatus_
    , _ciprsInstanceProfile = pInstanceProfile_
    }

-- | FIXME: Undocumented member.
ciprsStatus :: Lens' CreateInstanceProfileResponse Int
ciprsStatus = lens _ciprsStatus (\ s a -> s{_ciprsStatus = a});

-- | Information about the instance profile.
ciprsInstanceProfile :: Lens' CreateInstanceProfileResponse InstanceProfile
ciprsInstanceProfile = lens _ciprsInstanceProfile (\ s a -> s{_ciprsInstanceProfile = a});
