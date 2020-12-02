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
-- Module      : Network.AWS.APIGateway.UpdateClientCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an 'ClientCertificate' resource.
--
--
module Network.AWS.APIGateway.UpdateClientCertificate
    (
    -- * Creating a Request
      updateClientCertificate
    , UpdateClientCertificate
    -- * Request Lenses
    , uccPatchOperations
    , uccClientCertificateId

    -- * Destructuring the Response
    , clientCertificate
    , ClientCertificate
    -- * Response Lenses
    , ccPemEncodedCertificate
    , ccClientCertificateId
    , ccCreatedDate
    , ccExpirationDate
    , ccDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to change information about an 'ClientCertificate' resource.
--
--
--
-- /See:/ 'updateClientCertificate' smart constructor.
data UpdateClientCertificate = UpdateClientCertificate'
  { _uccPatchOperations     :: !(Maybe [PatchOperation])
  , _uccClientCertificateId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateClientCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccPatchOperations' - A list of update operations to be applied to the specified resource and in the order specified in this list.
--
-- * 'uccClientCertificateId' - [Required] The identifier of the 'ClientCertificate' resource to be updated.
updateClientCertificate
    :: Text -- ^ 'uccClientCertificateId'
    -> UpdateClientCertificate
updateClientCertificate pClientCertificateId_ =
  UpdateClientCertificate'
    { _uccPatchOperations = Nothing
    , _uccClientCertificateId = pClientCertificateId_
    }


-- | A list of update operations to be applied to the specified resource and in the order specified in this list.
uccPatchOperations :: Lens' UpdateClientCertificate [PatchOperation]
uccPatchOperations = lens _uccPatchOperations (\ s a -> s{_uccPatchOperations = a}) . _Default . _Coerce

-- | [Required] The identifier of the 'ClientCertificate' resource to be updated.
uccClientCertificateId :: Lens' UpdateClientCertificate Text
uccClientCertificateId = lens _uccClientCertificateId (\ s a -> s{_uccClientCertificateId = a})

instance AWSRequest UpdateClientCertificate where
        type Rs UpdateClientCertificate = ClientCertificate
        request = patchJSON apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateClientCertificate where

instance NFData UpdateClientCertificate where

instance ToHeaders UpdateClientCertificate where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToJSON UpdateClientCertificate where
        toJSON UpdateClientCertificate'{..}
          = object
              (catMaybes
                 [("patchOperations" .=) <$> _uccPatchOperations])

instance ToPath UpdateClientCertificate where
        toPath UpdateClientCertificate'{..}
          = mconcat
              ["/clientcertificates/",
               toBS _uccClientCertificateId]

instance ToQuery UpdateClientCertificate where
        toQuery = const mempty
