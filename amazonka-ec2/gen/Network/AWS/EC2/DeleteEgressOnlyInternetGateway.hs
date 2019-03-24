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
-- Module      : Network.AWS.EC2.DeleteEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only internet gateway.
--
--
module Network.AWS.EC2.DeleteEgressOnlyInternetGateway
    (
    -- * Creating a Request
      deleteEgressOnlyInternetGateway
    , DeleteEgressOnlyInternetGateway
    -- * Request Lenses
    , deoigeDryRun
    , deoigeEgressOnlyInternetGatewayId

    -- * Destructuring the Response
    , deleteEgressOnlyInternetGatewayResponse
    , DeleteEgressOnlyInternetGatewayResponse
    -- * Response Lenses
    , deoigersReturnCode
    , deoigersResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'
  { _deoigeDryRun                      :: !(Maybe Bool)
  , _deoigeEgressOnlyInternetGatewayId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoigeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'deoigeEgressOnlyInternetGatewayId' - The ID of the egress-only internet gateway.
deleteEgressOnlyInternetGateway
    :: Text -- ^ 'deoigeEgressOnlyInternetGatewayId'
    -> DeleteEgressOnlyInternetGateway
deleteEgressOnlyInternetGateway pEgressOnlyInternetGatewayId_ =
  DeleteEgressOnlyInternetGateway'
    { _deoigeDryRun = Nothing
    , _deoigeEgressOnlyInternetGatewayId = pEgressOnlyInternetGatewayId_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deoigeDryRun :: Lens' DeleteEgressOnlyInternetGateway (Maybe Bool)
deoigeDryRun = lens _deoigeDryRun (\ s a -> s{_deoigeDryRun = a})

-- | The ID of the egress-only internet gateway.
deoigeEgressOnlyInternetGatewayId :: Lens' DeleteEgressOnlyInternetGateway Text
deoigeEgressOnlyInternetGatewayId = lens _deoigeEgressOnlyInternetGatewayId (\ s a -> s{_deoigeEgressOnlyInternetGatewayId = a})

instance AWSRequest DeleteEgressOnlyInternetGateway
         where
        type Rs DeleteEgressOnlyInternetGateway =
             DeleteEgressOnlyInternetGatewayResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteEgressOnlyInternetGatewayResponse' <$>
                   (x .@? "returnCode") <*> (pure (fromEnum s)))

instance Hashable DeleteEgressOnlyInternetGateway
         where

instance NFData DeleteEgressOnlyInternetGateway where

instance ToHeaders DeleteEgressOnlyInternetGateway
         where
        toHeaders = const mempty

instance ToPath DeleteEgressOnlyInternetGateway where
        toPath = const "/"

instance ToQuery DeleteEgressOnlyInternetGateway
         where
        toQuery DeleteEgressOnlyInternetGateway'{..}
          = mconcat
              ["Action" =:
                 ("DeleteEgressOnlyInternetGateway" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _deoigeDryRun,
               "EgressOnlyInternetGatewayId" =:
                 _deoigeEgressOnlyInternetGatewayId]

-- | /See:/ 'deleteEgressOnlyInternetGatewayResponse' smart constructor.
data DeleteEgressOnlyInternetGatewayResponse = DeleteEgressOnlyInternetGatewayResponse'
  { _deoigersReturnCode     :: !(Maybe Bool)
  , _deoigersResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEgressOnlyInternetGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoigersReturnCode' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'deoigersResponseStatus' - -- | The response status code.
deleteEgressOnlyInternetGatewayResponse
    :: Int -- ^ 'deoigersResponseStatus'
    -> DeleteEgressOnlyInternetGatewayResponse
deleteEgressOnlyInternetGatewayResponse pResponseStatus_ =
  DeleteEgressOnlyInternetGatewayResponse'
    {_deoigersReturnCode = Nothing, _deoigersResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
deoigersReturnCode :: Lens' DeleteEgressOnlyInternetGatewayResponse (Maybe Bool)
deoigersReturnCode = lens _deoigersReturnCode (\ s a -> s{_deoigersReturnCode = a})

-- | -- | The response status code.
deoigersResponseStatus :: Lens' DeleteEgressOnlyInternetGatewayResponse Int
deoigersResponseStatus = lens _deoigersResponseStatus (\ s a -> s{_deoigersResponseStatus = a})

instance NFData
           DeleteEgressOnlyInternetGatewayResponse
         where
