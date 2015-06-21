{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.ReplaceNetworkACLAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Changes which network ACL a subnet is associated with. By default when
-- you create a subnet, it\'s automatically associated with the default
-- network ACL. For more information about network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkACLAssociation.html>
module Network.AWS.EC2.ReplaceNetworkACLAssociation
    (
    -- * Request
      ReplaceNetworkACLAssociation
    -- ** Request constructor
    , replaceNetworkACLAssociation
    -- ** Request lenses
    , rnaaDryRun
    , rnaaAssociationId
    , rnaaNetworkACLId

    -- * Response
    , ReplaceNetworkACLAssociationResponse
    -- ** Response constructor
    , replaceNetworkACLAssociationResponse
    -- ** Response lenses
    , rnaarNewAssociationId
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'replaceNetworkACLAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaaDryRun'
--
-- * 'rnaaAssociationId'
--
-- * 'rnaaNetworkACLId'
data ReplaceNetworkACLAssociation = ReplaceNetworkACLAssociation'{_rnaaDryRun :: Maybe Bool, _rnaaAssociationId :: Text, _rnaaNetworkACLId :: Text} deriving (Eq, Read, Show)

-- | 'ReplaceNetworkACLAssociation' smart constructor.
replaceNetworkACLAssociation :: Text -> Text -> ReplaceNetworkACLAssociation
replaceNetworkACLAssociation pAssociationId pNetworkACLId = ReplaceNetworkACLAssociation'{_rnaaDryRun = Nothing, _rnaaAssociationId = pAssociationId, _rnaaNetworkACLId = pNetworkACLId};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rnaaDryRun :: Lens' ReplaceNetworkACLAssociation (Maybe Bool)
rnaaDryRun = lens _rnaaDryRun (\ s a -> s{_rnaaDryRun = a});

-- | The ID of the current association between the original network ACL and
-- the subnet.
rnaaAssociationId :: Lens' ReplaceNetworkACLAssociation Text
rnaaAssociationId = lens _rnaaAssociationId (\ s a -> s{_rnaaAssociationId = a});

-- | The ID of the new network ACL to associate with the subnet.
rnaaNetworkACLId :: Lens' ReplaceNetworkACLAssociation Text
rnaaNetworkACLId = lens _rnaaNetworkACLId (\ s a -> s{_rnaaNetworkACLId = a});

instance AWSRequest ReplaceNetworkACLAssociation
         where
        type Sv ReplaceNetworkACLAssociation = EC2
        type Rs ReplaceNetworkACLAssociation =
             ReplaceNetworkACLAssociationResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ReplaceNetworkACLAssociationResponse' <$>
                   (x .@? "newAssociationId"))

instance ToHeaders ReplaceNetworkACLAssociation where
        toHeaders = const mempty

instance ToPath ReplaceNetworkACLAssociation where
        toPath = const "/"

instance ToQuery ReplaceNetworkACLAssociation where
        toQuery ReplaceNetworkACLAssociation'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceNetworkACLAssociation" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rnaaDryRun,
               "AssociationId" =: _rnaaAssociationId,
               "NetworkAclId" =: _rnaaNetworkACLId]

-- | /See:/ 'replaceNetworkACLAssociationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaarNewAssociationId'
newtype ReplaceNetworkACLAssociationResponse = ReplaceNetworkACLAssociationResponse'{_rnaarNewAssociationId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ReplaceNetworkACLAssociationResponse' smart constructor.
replaceNetworkACLAssociationResponse :: ReplaceNetworkACLAssociationResponse
replaceNetworkACLAssociationResponse = ReplaceNetworkACLAssociationResponse'{_rnaarNewAssociationId = Nothing};

-- | The ID of the new association.
rnaarNewAssociationId :: Lens' ReplaceNetworkACLAssociationResponse (Maybe Text)
rnaarNewAssociationId = lens _rnaarNewAssociationId (\ s a -> s{_rnaarNewAssociationId = a});
