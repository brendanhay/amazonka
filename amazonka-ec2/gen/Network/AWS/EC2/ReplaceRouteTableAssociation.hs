{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Changes the route table associated with a given subnet in a VPC. After
-- the operation completes, the subnet uses the routes in the new route
-- table it\'s associated with. For more information about route tables,
-- see
-- <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- You can also use ReplaceRouteTableAssociation to change which table is
-- the main route table in the VPC. You just specify the main route
-- table\'s association ID and the route table to be the new main route
-- table.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceRouteTableAssociation.html>
module Network.AWS.EC2.ReplaceRouteTableAssociation
    (
    -- * Request
      ReplaceRouteTableAssociation
    -- ** Request constructor
    , replaceRouteTableAssociation
    -- ** Request lenses
    , rrtaDryRun
    , rrtaAssociationId
    , rrtaRouteTableId

    -- * Response
    , ReplaceRouteTableAssociationResponse
    -- ** Response constructor
    , replaceRouteTableAssociationResponse
    -- ** Response lenses
    , rrtarNewAssociationId
    , rrtarStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'replaceRouteTableAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrtaDryRun'
--
-- * 'rrtaAssociationId'
--
-- * 'rrtaRouteTableId'
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation'
    { _rrtaDryRun        :: !(Maybe Bool)
    , _rrtaAssociationId :: !Text
    , _rrtaRouteTableId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceRouteTableAssociation' smart constructor.
replaceRouteTableAssociation :: Text -> Text -> ReplaceRouteTableAssociation
replaceRouteTableAssociation pAssociationId pRouteTableId =
    ReplaceRouteTableAssociation'
    { _rrtaDryRun = Nothing
    , _rrtaAssociationId = pAssociationId
    , _rrtaRouteTableId = pRouteTableId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rrtaDryRun :: Lens' ReplaceRouteTableAssociation (Maybe Bool)
rrtaDryRun = lens _rrtaDryRun (\ s a -> s{_rrtaDryRun = a});

-- | The association ID.
rrtaAssociationId :: Lens' ReplaceRouteTableAssociation Text
rrtaAssociationId = lens _rrtaAssociationId (\ s a -> s{_rrtaAssociationId = a});

-- | The ID of the new route table to associate with the subnet.
rrtaRouteTableId :: Lens' ReplaceRouteTableAssociation Text
rrtaRouteTableId = lens _rrtaRouteTableId (\ s a -> s{_rrtaRouteTableId = a});

instance AWSRequest ReplaceRouteTableAssociation
         where
        type Sv ReplaceRouteTableAssociation = EC2
        type Rs ReplaceRouteTableAssociation =
             ReplaceRouteTableAssociationResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ReplaceRouteTableAssociationResponse' <$>
                   (x .@? "newAssociationId") <*> (pure (fromEnum s)))

instance ToHeaders ReplaceRouteTableAssociation where
        toHeaders = const mempty

instance ToPath ReplaceRouteTableAssociation where
        toPath = const "/"

instance ToQuery ReplaceRouteTableAssociation where
        toQuery ReplaceRouteTableAssociation'{..}
          = mconcat
              ["Action" =:
                 ("ReplaceRouteTableAssociation" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _rrtaDryRun,
               "AssociationId" =: _rrtaAssociationId,
               "RouteTableId" =: _rrtaRouteTableId]

-- | /See:/ 'replaceRouteTableAssociationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrtarNewAssociationId'
--
-- * 'rrtarStatus'
data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse'
    { _rrtarNewAssociationId :: !(Maybe Text)
    , _rrtarStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceRouteTableAssociationResponse' smart constructor.
replaceRouteTableAssociationResponse :: Int -> ReplaceRouteTableAssociationResponse
replaceRouteTableAssociationResponse pStatus =
    ReplaceRouteTableAssociationResponse'
    { _rrtarNewAssociationId = Nothing
    , _rrtarStatus = pStatus
    }

-- | The ID of the new association.
rrtarNewAssociationId :: Lens' ReplaceRouteTableAssociationResponse (Maybe Text)
rrtarNewAssociationId = lens _rrtarNewAssociationId (\ s a -> s{_rrtarNewAssociationId = a});

-- | FIXME: Undocumented member.
rrtarStatus :: Lens' ReplaceRouteTableAssociationResponse Int
rrtarStatus = lens _rrtarStatus (\ s a -> s{_rrtarStatus = a});
