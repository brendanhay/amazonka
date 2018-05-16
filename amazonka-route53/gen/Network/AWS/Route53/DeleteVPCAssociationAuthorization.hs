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
-- Module      : Network.AWS.Route53.DeleteVPCAssociationAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes authorization to submit an @AssociateVPCWithHostedZone@ request to associate a specified VPC with a hosted zone that was created by a different account. You must use the account that created the hosted zone to submit a @DeleteVPCAssociationAuthorization@ request.
--
--
-- /Important:/ Sending this request only prevents the AWS account that created the VPC from associating the VPC with the Amazon Route 53 hosted zone in the future. If the VPC is already associated with the hosted zone, @DeleteVPCAssociationAuthorization@ won't disassociate the VPC from the hosted zone. If you want to delete an existing association, use @DisassociateVPCFromHostedZone@ .
--
module Network.AWS.Route53.DeleteVPCAssociationAuthorization
    (
    -- * Creating a Request
      deleteVPCAssociationAuthorization
    , DeleteVPCAssociationAuthorization
    -- * Request Lenses
    , dvaaHostedZoneId
    , dvaaVPC

    -- * Destructuring the Response
    , deleteVPCAssociationAuthorizationResponse
    , DeleteVPCAssociationAuthorizationResponse
    -- * Response Lenses
    , dvaarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to remove authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account.
--
--
--
-- /See:/ 'deleteVPCAssociationAuthorization' smart constructor.
data DeleteVPCAssociationAuthorization = DeleteVPCAssociationAuthorization'
  { _dvaaHostedZoneId :: !ResourceId
  , _dvaaVPC          :: !VPC
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCAssociationAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvaaHostedZoneId' - When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, the ID of the hosted zone.
--
-- * 'dvaaVPC' - When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, a complex type that includes the ID and region of the VPC.
deleteVPCAssociationAuthorization
    :: ResourceId -- ^ 'dvaaHostedZoneId'
    -> VPC -- ^ 'dvaaVPC'
    -> DeleteVPCAssociationAuthorization
deleteVPCAssociationAuthorization pHostedZoneId_ pVPC_ =
  DeleteVPCAssociationAuthorization'
    {_dvaaHostedZoneId = pHostedZoneId_, _dvaaVPC = pVPC_}


-- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, the ID of the hosted zone.
dvaaHostedZoneId :: Lens' DeleteVPCAssociationAuthorization ResourceId
dvaaHostedZoneId = lens _dvaaHostedZoneId (\ s a -> s{_dvaaHostedZoneId = a})

-- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, a complex type that includes the ID and region of the VPC.
dvaaVPC :: Lens' DeleteVPCAssociationAuthorization VPC
dvaaVPC = lens _dvaaVPC (\ s a -> s{_dvaaVPC = a})

instance AWSRequest DeleteVPCAssociationAuthorization
         where
        type Rs DeleteVPCAssociationAuthorization =
             DeleteVPCAssociationAuthorizationResponse
        request = postXML route53
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteVPCAssociationAuthorizationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteVPCAssociationAuthorization
         where

instance NFData DeleteVPCAssociationAuthorization
         where

instance ToElement DeleteVPCAssociationAuthorization
         where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}DeleteVPCAssociationAuthorizationRequest"

instance ToHeaders DeleteVPCAssociationAuthorization
         where
        toHeaders = const mempty

instance ToPath DeleteVPCAssociationAuthorization
         where
        toPath DeleteVPCAssociationAuthorization'{..}
          = mconcat
              ["/2013-04-01/hostedzone/", toBS _dvaaHostedZoneId,
               "/deauthorizevpcassociation"]

instance ToQuery DeleteVPCAssociationAuthorization
         where
        toQuery = const mempty

instance ToXML DeleteVPCAssociationAuthorization
         where
        toXML DeleteVPCAssociationAuthorization'{..}
          = mconcat ["VPC" @= _dvaaVPC]

-- | Empty response for the request.
--
--
--
-- /See:/ 'deleteVPCAssociationAuthorizationResponse' smart constructor.
newtype DeleteVPCAssociationAuthorizationResponse = DeleteVPCAssociationAuthorizationResponse'
  { _dvaarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCAssociationAuthorizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvaarsResponseStatus' - -- | The response status code.
deleteVPCAssociationAuthorizationResponse
    :: Int -- ^ 'dvaarsResponseStatus'
    -> DeleteVPCAssociationAuthorizationResponse
deleteVPCAssociationAuthorizationResponse pResponseStatus_ =
  DeleteVPCAssociationAuthorizationResponse'
    {_dvaarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dvaarsResponseStatus :: Lens' DeleteVPCAssociationAuthorizationResponse Int
dvaarsResponseStatus = lens _dvaarsResponseStatus (\ s a -> s{_dvaarsResponseStatus = a})

instance NFData
           DeleteVPCAssociationAuthorizationResponse
         where
