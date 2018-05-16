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
-- Module      : Network.AWS.EC2.ReplaceNetworkACLAssociation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes which network ACL a subnet is associated with. By default when you create a subnet, it's automatically associated with the default network ACL. For more information about network ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
--
-- This is an idempotent operation.
--
module Network.AWS.EC2.ReplaceNetworkACLAssociation
    (
    -- * Creating a Request
      replaceNetworkACLAssociation
    , ReplaceNetworkACLAssociation
    -- * Request Lenses
    , rnaaDryRun
    , rnaaAssociationId
    , rnaaNetworkACLId

    -- * Destructuring the Response
    , replaceNetworkACLAssociationResponse
    , ReplaceNetworkACLAssociationResponse
    -- * Response Lenses
    , rnaarsNewAssociationId
    , rnaarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ReplaceNetworkAclAssociation.
--
--
--
-- /See:/ 'replaceNetworkACLAssociation' smart constructor.
data ReplaceNetworkACLAssociation = ReplaceNetworkACLAssociation'
  { _rnaaDryRun        :: !(Maybe Bool)
  , _rnaaAssociationId :: !Text
  , _rnaaNetworkACLId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceNetworkACLAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnaaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rnaaAssociationId' - The ID of the current association between the original network ACL and the subnet.
--
-- * 'rnaaNetworkACLId' - The ID of the new network ACL to associate with the subnet.
replaceNetworkACLAssociation
    :: Text -- ^ 'rnaaAssociationId'
    -> Text -- ^ 'rnaaNetworkACLId'
    -> ReplaceNetworkACLAssociation
replaceNetworkACLAssociation pAssociationId_ pNetworkACLId_ =
  ReplaceNetworkACLAssociation'
    { _rnaaDryRun = Nothing
    , _rnaaAssociationId = pAssociationId_
    , _rnaaNetworkACLId = pNetworkACLId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rnaaDryRun :: Lens' ReplaceNetworkACLAssociation (Maybe Bool)
rnaaDryRun = lens _rnaaDryRun (\ s a -> s{_rnaaDryRun = a})

-- | The ID of the current association between the original network ACL and the subnet.
rnaaAssociationId :: Lens' ReplaceNetworkACLAssociation Text
rnaaAssociationId = lens _rnaaAssociationId (\ s a -> s{_rnaaAssociationId = a})

-- | The ID of the new network ACL to associate with the subnet.
rnaaNetworkACLId :: Lens' ReplaceNetworkACLAssociation Text
rnaaNetworkACLId = lens _rnaaNetworkACLId (\ s a -> s{_rnaaNetworkACLId = a})

instance AWSRequest ReplaceNetworkACLAssociation
         where
        type Rs ReplaceNetworkACLAssociation =
             ReplaceNetworkACLAssociationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ReplaceNetworkACLAssociationResponse' <$>
                   (x .@? "newAssociationId") <*> (pure (fromEnum s)))

instance Hashable ReplaceNetworkACLAssociation where

instance NFData ReplaceNetworkACLAssociation where

instance ToHeaders ReplaceNetworkACLAssociation where
        toHeaders = const mempty

instance ToPath ReplaceNetworkACLAssociation where
        toPath = const "/"

instance ToQuery ReplaceNetworkACLAssociation where
        toQuery ReplaceNetworkACLAssociation'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceNetworkAclAssociation" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _rnaaDryRun,
               "AssociationId" =: _rnaaAssociationId,
               "NetworkAclId" =: _rnaaNetworkACLId]

-- | Contains the output of ReplaceNetworkAclAssociation.
--
--
--
-- /See:/ 'replaceNetworkACLAssociationResponse' smart constructor.
data ReplaceNetworkACLAssociationResponse = ReplaceNetworkACLAssociationResponse'
  { _rnaarsNewAssociationId :: !(Maybe Text)
  , _rnaarsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplaceNetworkACLAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnaarsNewAssociationId' - The ID of the new association.
--
-- * 'rnaarsResponseStatus' - -- | The response status code.
replaceNetworkACLAssociationResponse
    :: Int -- ^ 'rnaarsResponseStatus'
    -> ReplaceNetworkACLAssociationResponse
replaceNetworkACLAssociationResponse pResponseStatus_ =
  ReplaceNetworkACLAssociationResponse'
    { _rnaarsNewAssociationId = Nothing
    , _rnaarsResponseStatus = pResponseStatus_
    }


-- | The ID of the new association.
rnaarsNewAssociationId :: Lens' ReplaceNetworkACLAssociationResponse (Maybe Text)
rnaarsNewAssociationId = lens _rnaarsNewAssociationId (\ s a -> s{_rnaarsNewAssociationId = a})

-- | -- | The response status code.
rnaarsResponseStatus :: Lens' ReplaceNetworkACLAssociationResponse Int
rnaarsResponseStatus = lens _rnaarsResponseStatus (\ s a -> s{_rnaarsResponseStatus = a})

instance NFData ReplaceNetworkACLAssociationResponse
         where
