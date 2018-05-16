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
-- Module      : Network.AWS.APIGateway.DeleteClientCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the 'ClientCertificate' resource.
--
--
module Network.AWS.APIGateway.DeleteClientCertificate
    (
    -- * Creating a Request
      deleteClientCertificate
    , DeleteClientCertificate
    -- * Request Lenses
    , dccClientCertificateId

    -- * Destructuring the Response
    , deleteClientCertificateResponse
    , DeleteClientCertificateResponse
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to delete the 'ClientCertificate' resource.
--
--
--
-- /See:/ 'deleteClientCertificate' smart constructor.
newtype DeleteClientCertificate = DeleteClientCertificate'
  { _dccClientCertificateId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccClientCertificateId' - [Required] The identifier of the 'ClientCertificate' resource to be deleted.
deleteClientCertificate
    :: Text -- ^ 'dccClientCertificateId'
    -> DeleteClientCertificate
deleteClientCertificate pClientCertificateId_ =
  DeleteClientCertificate' {_dccClientCertificateId = pClientCertificateId_}


-- | [Required] The identifier of the 'ClientCertificate' resource to be deleted.
dccClientCertificateId :: Lens' DeleteClientCertificate Text
dccClientCertificateId = lens _dccClientCertificateId (\ s a -> s{_dccClientCertificateId = a})

instance AWSRequest DeleteClientCertificate where
        type Rs DeleteClientCertificate =
             DeleteClientCertificateResponse
        request = delete apiGateway
        response
          = receiveNull DeleteClientCertificateResponse'

instance Hashable DeleteClientCertificate where

instance NFData DeleteClientCertificate where

instance ToHeaders DeleteClientCertificate where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath DeleteClientCertificate where
        toPath DeleteClientCertificate'{..}
          = mconcat
              ["/clientcertificates/",
               toBS _dccClientCertificateId]

instance ToQuery DeleteClientCertificate where
        toQuery = const mempty

-- | /See:/ 'deleteClientCertificateResponse' smart constructor.
data DeleteClientCertificateResponse =
  DeleteClientCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClientCertificateResponse' with the minimum fields required to make a request.
--
deleteClientCertificateResponse
    :: DeleteClientCertificateResponse
deleteClientCertificateResponse = DeleteClientCertificateResponse'


instance NFData DeleteClientCertificateResponse where
