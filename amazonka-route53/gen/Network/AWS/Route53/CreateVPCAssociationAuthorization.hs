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
-- Module      : Network.AWS.Route53.CreateVPCAssociationAuthorization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the AWS account that created a specified VPC to submit an @AssociateVPCWithHostedZone@ request to associate the VPC with a specified hosted zone that was created by a different account. To submit a @CreateVPCAssociationAuthorization@ request, you must use the account that created the hosted zone. After you authorize the association, use the account that created the VPC to submit an @AssociateVPCWithHostedZone@ request.
--
--
module Network.AWS.Route53.CreateVPCAssociationAuthorization
    (
    -- * Creating a Request
      createVPCAssociationAuthorization
    , CreateVPCAssociationAuthorization
    -- * Request Lenses
    , cvaaHostedZoneId
    , cvaaVPC

    -- * Destructuring the Response
    , createVPCAssociationAuthorizationResponse
    , CreateVPCAssociationAuthorizationResponse
    -- * Response Lenses
    , cvaarsResponseStatus
    , cvaarsHostedZoneId
    , cvaarsVPC
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to authorize associating a VPC with your private hosted zone. Authorization is only required when a private hosted zone and a VPC were created by using different accounts.
--
--
--
-- /See:/ 'createVPCAssociationAuthorization' smart constructor.
data CreateVPCAssociationAuthorization = CreateVPCAssociationAuthorization'
  { _cvaaHostedZoneId :: !ResourceId
  , _cvaaVPC          :: !VPC
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCAssociationAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvaaHostedZoneId' - The ID of the private hosted zone that you want to authorize associating a VPC with.
--
-- * 'cvaaVPC' - A complex type that contains the VPC ID and region for the VPC that you want to authorize associating with your hosted zone.
createVPCAssociationAuthorization
    :: ResourceId -- ^ 'cvaaHostedZoneId'
    -> VPC -- ^ 'cvaaVPC'
    -> CreateVPCAssociationAuthorization
createVPCAssociationAuthorization pHostedZoneId_ pVPC_ =
  CreateVPCAssociationAuthorization'
    {_cvaaHostedZoneId = pHostedZoneId_, _cvaaVPC = pVPC_}


-- | The ID of the private hosted zone that you want to authorize associating a VPC with.
cvaaHostedZoneId :: Lens' CreateVPCAssociationAuthorization ResourceId
cvaaHostedZoneId = lens _cvaaHostedZoneId (\ s a -> s{_cvaaHostedZoneId = a})

-- | A complex type that contains the VPC ID and region for the VPC that you want to authorize associating with your hosted zone.
cvaaVPC :: Lens' CreateVPCAssociationAuthorization VPC
cvaaVPC = lens _cvaaVPC (\ s a -> s{_cvaaVPC = a})

instance AWSRequest CreateVPCAssociationAuthorization
         where
        type Rs CreateVPCAssociationAuthorization =
             CreateVPCAssociationAuthorizationResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCAssociationAuthorizationResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "HostedZoneId") <*>
                     (x .@ "VPC"))

instance Hashable CreateVPCAssociationAuthorization
         where

instance NFData CreateVPCAssociationAuthorization
         where

instance ToElement CreateVPCAssociationAuthorization
         where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}CreateVPCAssociationAuthorizationRequest"

instance ToHeaders CreateVPCAssociationAuthorization
         where
        toHeaders = const mempty

instance ToPath CreateVPCAssociationAuthorization
         where
        toPath CreateVPCAssociationAuthorization'{..}
          = mconcat
              ["/2013-04-01/hostedzone/", toBS _cvaaHostedZoneId,
               "/authorizevpcassociation"]

instance ToQuery CreateVPCAssociationAuthorization
         where
        toQuery = const mempty

instance ToXML CreateVPCAssociationAuthorization
         where
        toXML CreateVPCAssociationAuthorization'{..}
          = mconcat ["VPC" @= _cvaaVPC]

-- | A complex type that contains the response information from a @CreateVPCAssociationAuthorization@ request.
--
--
--
-- /See:/ 'createVPCAssociationAuthorizationResponse' smart constructor.
data CreateVPCAssociationAuthorizationResponse = CreateVPCAssociationAuthorizationResponse'
  { _cvaarsResponseStatus :: !Int
  , _cvaarsHostedZoneId   :: !ResourceId
  , _cvaarsVPC            :: !VPC
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCAssociationAuthorizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvaarsResponseStatus' - -- | The response status code.
--
-- * 'cvaarsHostedZoneId' - The ID of the hosted zone that you authorized associating a VPC with.
--
-- * 'cvaarsVPC' - The VPC that you authorized associating with a hosted zone.
createVPCAssociationAuthorizationResponse
    :: Int -- ^ 'cvaarsResponseStatus'
    -> ResourceId -- ^ 'cvaarsHostedZoneId'
    -> VPC -- ^ 'cvaarsVPC'
    -> CreateVPCAssociationAuthorizationResponse
createVPCAssociationAuthorizationResponse pResponseStatus_ pHostedZoneId_ pVPC_ =
  CreateVPCAssociationAuthorizationResponse'
    { _cvaarsResponseStatus = pResponseStatus_
    , _cvaarsHostedZoneId = pHostedZoneId_
    , _cvaarsVPC = pVPC_
    }


-- | -- | The response status code.
cvaarsResponseStatus :: Lens' CreateVPCAssociationAuthorizationResponse Int
cvaarsResponseStatus = lens _cvaarsResponseStatus (\ s a -> s{_cvaarsResponseStatus = a})

-- | The ID of the hosted zone that you authorized associating a VPC with.
cvaarsHostedZoneId :: Lens' CreateVPCAssociationAuthorizationResponse ResourceId
cvaarsHostedZoneId = lens _cvaarsHostedZoneId (\ s a -> s{_cvaarsHostedZoneId = a})

-- | The VPC that you authorized associating with a hosted zone.
cvaarsVPC :: Lens' CreateVPCAssociationAuthorizationResponse VPC
cvaarsVPC = lens _cvaarsVPC (\ s a -> s{_cvaarsVPC = a})

instance NFData
           CreateVPCAssociationAuthorizationResponse
         where
