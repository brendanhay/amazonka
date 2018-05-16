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
-- Module      : Network.AWS.EC2.AssociateIAMInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an IAM instance profile with a running or stopped instance. You cannot associate more than one IAM instance profile with an instance.
--
--
module Network.AWS.EC2.AssociateIAMInstanceProfile
    (
    -- * Creating a Request
      associateIAMInstanceProfile
    , AssociateIAMInstanceProfile
    -- * Request Lenses
    , aiapIAMInstanceProfile
    , aiapInstanceId

    -- * Destructuring the Response
    , associateIAMInstanceProfileResponse
    , AssociateIAMInstanceProfileResponse
    -- * Response Lenses
    , aiaprsIAMInstanceProfileAssociation
    , aiaprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateIAMInstanceProfile' smart constructor.
data AssociateIAMInstanceProfile = AssociateIAMInstanceProfile'
  { _aiapIAMInstanceProfile :: !IAMInstanceProfileSpecification
  , _aiapInstanceId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateIAMInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiapIAMInstanceProfile' - The IAM instance profile.
--
-- * 'aiapInstanceId' - The ID of the instance.
associateIAMInstanceProfile
    :: IAMInstanceProfileSpecification -- ^ 'aiapIAMInstanceProfile'
    -> Text -- ^ 'aiapInstanceId'
    -> AssociateIAMInstanceProfile
associateIAMInstanceProfile pIAMInstanceProfile_ pInstanceId_ =
  AssociateIAMInstanceProfile'
    { _aiapIAMInstanceProfile = pIAMInstanceProfile_
    , _aiapInstanceId = pInstanceId_
    }


-- | The IAM instance profile.
aiapIAMInstanceProfile :: Lens' AssociateIAMInstanceProfile IAMInstanceProfileSpecification
aiapIAMInstanceProfile = lens _aiapIAMInstanceProfile (\ s a -> s{_aiapIAMInstanceProfile = a})

-- | The ID of the instance.
aiapInstanceId :: Lens' AssociateIAMInstanceProfile Text
aiapInstanceId = lens _aiapInstanceId (\ s a -> s{_aiapInstanceId = a})

instance AWSRequest AssociateIAMInstanceProfile where
        type Rs AssociateIAMInstanceProfile =
             AssociateIAMInstanceProfileResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AssociateIAMInstanceProfileResponse' <$>
                   (x .@? "iamInstanceProfileAssociation") <*>
                     (pure (fromEnum s)))

instance Hashable AssociateIAMInstanceProfile where

instance NFData AssociateIAMInstanceProfile where

instance ToHeaders AssociateIAMInstanceProfile where
        toHeaders = const mempty

instance ToPath AssociateIAMInstanceProfile where
        toPath = const "/"

instance ToQuery AssociateIAMInstanceProfile where
        toQuery AssociateIAMInstanceProfile'{..}
          = mconcat
              ["Action" =:
                 ("AssociateIamInstanceProfile" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "IamInstanceProfile" =: _aiapIAMInstanceProfile,
               "InstanceId" =: _aiapInstanceId]

-- | /See:/ 'associateIAMInstanceProfileResponse' smart constructor.
data AssociateIAMInstanceProfileResponse = AssociateIAMInstanceProfileResponse'
  { _aiaprsIAMInstanceProfileAssociation :: !(Maybe IAMInstanceProfileAssociation)
  , _aiaprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateIAMInstanceProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiaprsIAMInstanceProfileAssociation' - Information about the IAM instance profile association.
--
-- * 'aiaprsResponseStatus' - -- | The response status code.
associateIAMInstanceProfileResponse
    :: Int -- ^ 'aiaprsResponseStatus'
    -> AssociateIAMInstanceProfileResponse
associateIAMInstanceProfileResponse pResponseStatus_ =
  AssociateIAMInstanceProfileResponse'
    { _aiaprsIAMInstanceProfileAssociation = Nothing
    , _aiaprsResponseStatus = pResponseStatus_
    }


-- | Information about the IAM instance profile association.
aiaprsIAMInstanceProfileAssociation :: Lens' AssociateIAMInstanceProfileResponse (Maybe IAMInstanceProfileAssociation)
aiaprsIAMInstanceProfileAssociation = lens _aiaprsIAMInstanceProfileAssociation (\ s a -> s{_aiaprsIAMInstanceProfileAssociation = a})

-- | -- | The response status code.
aiaprsResponseStatus :: Lens' AssociateIAMInstanceProfileResponse Int
aiaprsResponseStatus = lens _aiaprsResponseStatus (\ s a -> s{_aiaprsResponseStatus = a})

instance NFData AssociateIAMInstanceProfileResponse
         where
