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
-- Module      : Network.AWS.EC2.DisassociateIAMInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an IAM instance profile from a running or stopped instance.
--
--
-- Use 'DescribeIamInstanceProfileAssociations' to get the association ID.
--
module Network.AWS.EC2.DisassociateIAMInstanceProfile
    (
    -- * Creating a Request
      disassociateIAMInstanceProfile
    , DisassociateIAMInstanceProfile
    -- * Request Lenses
    , diapAssociationId

    -- * Destructuring the Response
    , disassociateIAMInstanceProfileResponse
    , DisassociateIAMInstanceProfileResponse
    -- * Response Lenses
    , diaprsIAMInstanceProfileAssociation
    , diaprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateIAMInstanceProfile' smart constructor.
newtype DisassociateIAMInstanceProfile = DisassociateIAMInstanceProfile'
  { _diapAssociationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateIAMInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diapAssociationId' - The ID of the IAM instance profile association.
disassociateIAMInstanceProfile
    :: Text -- ^ 'diapAssociationId'
    -> DisassociateIAMInstanceProfile
disassociateIAMInstanceProfile pAssociationId_ =
  DisassociateIAMInstanceProfile' {_diapAssociationId = pAssociationId_}


-- | The ID of the IAM instance profile association.
diapAssociationId :: Lens' DisassociateIAMInstanceProfile Text
diapAssociationId = lens _diapAssociationId (\ s a -> s{_diapAssociationId = a})

instance AWSRequest DisassociateIAMInstanceProfile
         where
        type Rs DisassociateIAMInstanceProfile =
             DisassociateIAMInstanceProfileResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DisassociateIAMInstanceProfileResponse' <$>
                   (x .@? "iamInstanceProfileAssociation") <*>
                     (pure (fromEnum s)))

instance Hashable DisassociateIAMInstanceProfile
         where

instance NFData DisassociateIAMInstanceProfile where

instance ToHeaders DisassociateIAMInstanceProfile
         where
        toHeaders = const mempty

instance ToPath DisassociateIAMInstanceProfile where
        toPath = const "/"

instance ToQuery DisassociateIAMInstanceProfile where
        toQuery DisassociateIAMInstanceProfile'{..}
          = mconcat
              ["Action" =:
                 ("DisassociateIamInstanceProfile" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AssociationId" =: _diapAssociationId]

-- | /See:/ 'disassociateIAMInstanceProfileResponse' smart constructor.
data DisassociateIAMInstanceProfileResponse = DisassociateIAMInstanceProfileResponse'
  { _diaprsIAMInstanceProfileAssociation :: !(Maybe IAMInstanceProfileAssociation)
  , _diaprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateIAMInstanceProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diaprsIAMInstanceProfileAssociation' - Information about the IAM instance profile association.
--
-- * 'diaprsResponseStatus' - -- | The response status code.
disassociateIAMInstanceProfileResponse
    :: Int -- ^ 'diaprsResponseStatus'
    -> DisassociateIAMInstanceProfileResponse
disassociateIAMInstanceProfileResponse pResponseStatus_ =
  DisassociateIAMInstanceProfileResponse'
    { _diaprsIAMInstanceProfileAssociation = Nothing
    , _diaprsResponseStatus = pResponseStatus_
    }


-- | Information about the IAM instance profile association.
diaprsIAMInstanceProfileAssociation :: Lens' DisassociateIAMInstanceProfileResponse (Maybe IAMInstanceProfileAssociation)
diaprsIAMInstanceProfileAssociation = lens _diaprsIAMInstanceProfileAssociation (\ s a -> s{_diaprsIAMInstanceProfileAssociation = a})

-- | -- | The response status code.
diaprsResponseStatus :: Lens' DisassociateIAMInstanceProfileResponse Int
diaprsResponseStatus = lens _diaprsResponseStatus (\ s a -> s{_diaprsResponseStatus = a})

instance NFData
           DisassociateIAMInstanceProfileResponse
         where
