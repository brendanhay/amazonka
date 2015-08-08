{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceNetworkACLAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes which network ACL a subnet is associated with. By default when
-- you create a subnet, it\'s automatically associated with the default
-- network ACL. For more information about network ACLs, see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkACLAssociation.html AWS API Reference> for ReplaceNetworkACLAssociation.
module Network.AWS.EC2.ReplaceNetworkACLAssociation
    (
    -- * Creating a Request
      ReplaceNetworkACLAssociation
    , replaceNetworkACLAssociation
    -- * Request Lenses
    , rnaaDryRun
    , rnaaAssociationId
    , rnaaNetworkACLId

    -- * Destructuring the Response
    , ReplaceNetworkACLAssociationResponse
    , replaceNetworkACLAssociationResponse
    -- * Response Lenses
    , rnaarsNewAssociationId
    , rnaarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'replaceNetworkACLAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaaDryRun'
--
-- * 'rnaaAssociationId'
--
-- * 'rnaaNetworkACLId'
data ReplaceNetworkACLAssociation = ReplaceNetworkACLAssociation'
    { _rnaaDryRun        :: !(Maybe Bool)
    , _rnaaAssociationId :: !Text
    , _rnaaNetworkACLId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceNetworkACLAssociation' smart constructor.
replaceNetworkACLAssociation :: Text -> Text -> ReplaceNetworkACLAssociation
replaceNetworkACLAssociation pAssociationId_ pNetworkACLId_ =
    ReplaceNetworkACLAssociation'
    { _rnaaDryRun = Nothing
    , _rnaaAssociationId = pAssociationId_
    , _rnaaNetworkACLId = pNetworkACLId_
    }

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
                   (x .@? "newAssociationId") <*> (pure (fromEnum s)))

instance ToHeaders ReplaceNetworkACLAssociation where
        toHeaders = const mempty

instance ToPath ReplaceNetworkACLAssociation where
        toPath = const "/"

instance ToQuery ReplaceNetworkACLAssociation where
        toQuery ReplaceNetworkACLAssociation'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceNetworkAclAssociation" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rnaaDryRun,
               "AssociationId" =: _rnaaAssociationId,
               "NetworkAclId" =: _rnaaNetworkACLId]

-- | /See:/ 'replaceNetworkACLAssociationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaarsNewAssociationId'
--
-- * 'rnaarsStatus'
data ReplaceNetworkACLAssociationResponse = ReplaceNetworkACLAssociationResponse'
    { _rnaarsNewAssociationId :: !(Maybe Text)
    , _rnaarsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceNetworkACLAssociationResponse' smart constructor.
replaceNetworkACLAssociationResponse :: Int -> ReplaceNetworkACLAssociationResponse
replaceNetworkACLAssociationResponse pStatus_ =
    ReplaceNetworkACLAssociationResponse'
    { _rnaarsNewAssociationId = Nothing
    , _rnaarsStatus = pStatus_
    }

-- | The ID of the new association.
rnaarsNewAssociationId :: Lens' ReplaceNetworkACLAssociationResponse (Maybe Text)
rnaarsNewAssociationId = lens _rnaarsNewAssociationId (\ s a -> s{_rnaarsNewAssociationId = a});

-- | Undocumented member.
rnaarsStatus :: Lens' ReplaceNetworkACLAssociationResponse Int
rnaarsStatus = lens _rnaarsStatus (\ s a -> s{_rnaarsStatus = a});
