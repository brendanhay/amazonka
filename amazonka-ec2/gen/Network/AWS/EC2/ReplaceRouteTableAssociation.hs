{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , rrtarqDryRun
    , rrtarqAssociationId
    , rrtarqRouteTableId

    -- * Response
    , ReplaceRouteTableAssociationResponse
    -- ** Response constructor
    , replaceRouteTableAssociationResponse
    -- ** Response lenses
    , rrtarsNewAssociationId
    , rrtarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'replaceRouteTableAssociation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrtarqDryRun'
--
-- * 'rrtarqAssociationId'
--
-- * 'rrtarqRouteTableId'
data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation'
    { _rrtarqDryRun        :: !(Maybe Bool)
    , _rrtarqAssociationId :: !Text
    , _rrtarqRouteTableId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceRouteTableAssociation' smart constructor.
replaceRouteTableAssociation :: Text -> Text -> ReplaceRouteTableAssociation
replaceRouteTableAssociation pAssociationId pRouteTableId =
    ReplaceRouteTableAssociation'
    { _rrtarqDryRun = Nothing
    , _rrtarqAssociationId = pAssociationId
    , _rrtarqRouteTableId = pRouteTableId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rrtarqDryRun :: Lens' ReplaceRouteTableAssociation (Maybe Bool)
rrtarqDryRun = lens _rrtarqDryRun (\ s a -> s{_rrtarqDryRun = a});

-- | The association ID.
rrtarqAssociationId :: Lens' ReplaceRouteTableAssociation Text
rrtarqAssociationId = lens _rrtarqAssociationId (\ s a -> s{_rrtarqAssociationId = a});

-- | The ID of the new route table to associate with the subnet.
rrtarqRouteTableId :: Lens' ReplaceRouteTableAssociation Text
rrtarqRouteTableId = lens _rrtarqRouteTableId (\ s a -> s{_rrtarqRouteTableId = a});

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
               "DryRun" =: _rrtarqDryRun,
               "AssociationId" =: _rrtarqAssociationId,
               "RouteTableId" =: _rrtarqRouteTableId]

-- | /See:/ 'replaceRouteTableAssociationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrtarsNewAssociationId'
--
-- * 'rrtarsStatus'
data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse'
    { _rrtarsNewAssociationId :: !(Maybe Text)
    , _rrtarsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReplaceRouteTableAssociationResponse' smart constructor.
replaceRouteTableAssociationResponse :: Int -> ReplaceRouteTableAssociationResponse
replaceRouteTableAssociationResponse pStatus =
    ReplaceRouteTableAssociationResponse'
    { _rrtarsNewAssociationId = Nothing
    , _rrtarsStatus = pStatus
    }

-- | The ID of the new association.
rrtarsNewAssociationId :: Lens' ReplaceRouteTableAssociationResponse (Maybe Text)
rrtarsNewAssociationId = lens _rrtarsNewAssociationId (\ s a -> s{_rrtarsNewAssociationId = a});

-- | FIXME: Undocumented member.
rrtarsStatus :: Lens' ReplaceRouteTableAssociationResponse Int
rrtarsStatus = lens _rrtarsStatus (\ s a -> s{_rrtarsStatus = a});
