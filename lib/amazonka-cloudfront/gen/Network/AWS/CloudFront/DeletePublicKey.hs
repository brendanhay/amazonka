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
-- Module      : Network.AWS.CloudFront.DeletePublicKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a public key you previously added to CloudFront.
--
--
module Network.AWS.CloudFront.DeletePublicKey
    (
    -- * Creating a Request
      deletePublicKey
    , DeletePublicKey
    -- * Request Lenses
    , dpkIfMatch
    , dpkId

    -- * Destructuring the Response
    , deletePublicKeyResponse
    , DeletePublicKeyResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePublicKey' smart constructor.
data DeletePublicKey = DeletePublicKey'
  { _dpkIfMatch :: !(Maybe Text)
  , _dpkId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpkIfMatch' - The value of the @ETag@ header that you received when retrieving the public key identity to delete. For example: @E2QWRUHAPOMQZL@ .
--
-- * 'dpkId' - The ID of the public key you want to remove from CloudFront.
deletePublicKey
    :: Text -- ^ 'dpkId'
    -> DeletePublicKey
deletePublicKey pId_ = DeletePublicKey' {_dpkIfMatch = Nothing, _dpkId = pId_}


-- | The value of the @ETag@ header that you received when retrieving the public key identity to delete. For example: @E2QWRUHAPOMQZL@ .
dpkIfMatch :: Lens' DeletePublicKey (Maybe Text)
dpkIfMatch = lens _dpkIfMatch (\ s a -> s{_dpkIfMatch = a})

-- | The ID of the public key you want to remove from CloudFront.
dpkId :: Lens' DeletePublicKey Text
dpkId = lens _dpkId (\ s a -> s{_dpkId = a})

instance AWSRequest DeletePublicKey where
        type Rs DeletePublicKey = DeletePublicKeyResponse
        request = delete cloudFront
        response = receiveNull DeletePublicKeyResponse'

instance Hashable DeletePublicKey where

instance NFData DeletePublicKey where

instance ToHeaders DeletePublicKey where
        toHeaders DeletePublicKey'{..}
          = mconcat ["If-Match" =# _dpkIfMatch]

instance ToPath DeletePublicKey where
        toPath DeletePublicKey'{..}
          = mconcat ["/2017-10-30/public-key/", toBS _dpkId]

instance ToQuery DeletePublicKey where
        toQuery = const mempty

-- | /See:/ 'deletePublicKeyResponse' smart constructor.
data DeletePublicKeyResponse =
  DeletePublicKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePublicKeyResponse' with the minimum fields required to make a request.
--
deletePublicKeyResponse
    :: DeletePublicKeyResponse
deletePublicKeyResponse = DeletePublicKeyResponse'


instance NFData DeletePublicKeyResponse where
