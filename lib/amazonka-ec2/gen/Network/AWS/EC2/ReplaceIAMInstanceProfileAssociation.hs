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
-- Module      : Network.AWS.EC2.ReplaceIAMInstanceProfileAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces an IAM instance profile for the specified running instance. You can use this action to change the IAM instance profile that's associated with an instance without having to disassociate the existing IAM instance profile first.
--
--
-- Use 'DescribeIamInstanceProfileAssociations' to get the association ID.
--
module Network.AWS.EC2.ReplaceIAMInstanceProfileAssociation
    (
    -- * Creating a Request
      replaceIAMInstanceProfileAssociation
    , ReplaceIAMInstanceProfileAssociation
    -- * Request Lenses
    , riapaIAMInstanceProfile
    , riapaAssociationId

    -- * Destructuring the Response
    , replaceIAMInstanceProfileAssociationResponse
    , ReplaceIAMInstanceProfileAssociationResponse
    -- * Response Lenses
    , riaparsIAMInstanceProfileAssociation
    , riaparsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'replaceIAMInstanceProfileAssociation' smart constructor.
data ReplaceIAMInstanceProfileAssociation = ReplaceIAMInstanceProfileAssociation'
  { _riapaIAMInstanceProfile :: !IAMInstanceProfileSpecification
  , _riapaAssociationId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceIAMInstanceProfileAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riapaIAMInstanceProfile' - The IAM instance profile.
--
-- * 'riapaAssociationId' - The ID of the existing IAM instance profile association.
replaceIAMInstanceProfileAssociation
    :: IAMInstanceProfileSpecification -- ^ 'riapaIAMInstanceProfile'
    -> Text -- ^ 'riapaAssociationId'
    -> ReplaceIAMInstanceProfileAssociation
replaceIAMInstanceProfileAssociation pIAMInstanceProfile_ pAssociationId_ =
  ReplaceIAMInstanceProfileAssociation'
    { _riapaIAMInstanceProfile = pIAMInstanceProfile_
    , _riapaAssociationId = pAssociationId_
    }


-- | The IAM instance profile.
riapaIAMInstanceProfile :: Lens' ReplaceIAMInstanceProfileAssociation IAMInstanceProfileSpecification
riapaIAMInstanceProfile = lens _riapaIAMInstanceProfile (\ s a -> s{_riapaIAMInstanceProfile = a})

-- | The ID of the existing IAM instance profile association.
riapaAssociationId :: Lens' ReplaceIAMInstanceProfileAssociation Text
riapaAssociationId = lens _riapaAssociationId (\ s a -> s{_riapaAssociationId = a})

instance AWSRequest
           ReplaceIAMInstanceProfileAssociation
         where
        type Rs ReplaceIAMInstanceProfileAssociation =
             ReplaceIAMInstanceProfileAssociationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ReplaceIAMInstanceProfileAssociationResponse' <$>
                   (x .@? "iamInstanceProfileAssociation") <*>
                     (pure (fromEnum s)))

instance Hashable
           ReplaceIAMInstanceProfileAssociation
         where

instance NFData ReplaceIAMInstanceProfileAssociation
         where

instance ToHeaders
           ReplaceIAMInstanceProfileAssociation
         where
        toHeaders = const mempty

instance ToPath ReplaceIAMInstanceProfileAssociation
         where
        toPath = const "/"

instance ToQuery ReplaceIAMInstanceProfileAssociation
         where
        toQuery ReplaceIAMInstanceProfileAssociation'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceIamInstanceProfileAssociation" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "IamInstanceProfile" =: _riapaIAMInstanceProfile,
               "AssociationId" =: _riapaAssociationId]

-- | /See:/ 'replaceIAMInstanceProfileAssociationResponse' smart constructor.
data ReplaceIAMInstanceProfileAssociationResponse = ReplaceIAMInstanceProfileAssociationResponse'
  { _riaparsIAMInstanceProfileAssociation :: !(Maybe IAMInstanceProfileAssociation)
  , _riaparsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceIAMInstanceProfileAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riaparsIAMInstanceProfileAssociation' - Information about the IAM instance profile association.
--
-- * 'riaparsResponseStatus' - -- | The response status code.
replaceIAMInstanceProfileAssociationResponse
    :: Int -- ^ 'riaparsResponseStatus'
    -> ReplaceIAMInstanceProfileAssociationResponse
replaceIAMInstanceProfileAssociationResponse pResponseStatus_ =
  ReplaceIAMInstanceProfileAssociationResponse'
    { _riaparsIAMInstanceProfileAssociation = Nothing
    , _riaparsResponseStatus = pResponseStatus_
    }


-- | Information about the IAM instance profile association.
riaparsIAMInstanceProfileAssociation :: Lens' ReplaceIAMInstanceProfileAssociationResponse (Maybe IAMInstanceProfileAssociation)
riaparsIAMInstanceProfileAssociation = lens _riaparsIAMInstanceProfileAssociation (\ s a -> s{_riaparsIAMInstanceProfileAssociation = a})

-- | -- | The response status code.
riaparsResponseStatus :: Lens' ReplaceIAMInstanceProfileAssociationResponse Int
riaparsResponseStatus = lens _riaparsResponseStatus (\ s a -> s{_riaparsResponseStatus = a})

instance NFData
           ReplaceIAMInstanceProfileAssociationResponse
         where
