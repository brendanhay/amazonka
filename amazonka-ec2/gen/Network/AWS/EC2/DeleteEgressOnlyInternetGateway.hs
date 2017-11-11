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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only Internet gateway.
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
    , delrsReturnCode
    , delrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'
  { _deoigeDryRun                      :: {-# NOUNPACK #-}!(Maybe Bool)
  , _deoigeEgressOnlyInternetGatewayId :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEgressOnlyInternetGateway' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deoigeDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'deoigeEgressOnlyInternetGatewayId' - The ID of the egress-only Internet gateway.
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
deoigeDryRun = lens _deoigeDryRun (\ s a -> s{_deoigeDryRun = a});

-- | The ID of the egress-only Internet gateway.
deoigeEgressOnlyInternetGatewayId :: Lens' DeleteEgressOnlyInternetGateway Text
deoigeEgressOnlyInternetGatewayId = lens _deoigeEgressOnlyInternetGatewayId (\ s a -> s{_deoigeEgressOnlyInternetGatewayId = a});

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
  { _delrsReturnCode     :: {-# NOUNPACK #-}!(Maybe Bool)
  , _delrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEgressOnlyInternetGatewayResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsReturnCode' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteEgressOnlyInternetGatewayResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteEgressOnlyInternetGatewayResponse
deleteEgressOnlyInternetGatewayResponse pResponseStatus_ =
  DeleteEgressOnlyInternetGatewayResponse'
  {_delrsReturnCode = Nothing, _delrsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
delrsReturnCode :: Lens' DeleteEgressOnlyInternetGatewayResponse (Maybe Bool)
delrsReturnCode = lens _delrsReturnCode (\ s a -> s{_delrsReturnCode = a});

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteEgressOnlyInternetGatewayResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a});

instance NFData
           DeleteEgressOnlyInternetGatewayResponse
         where
