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
-- Module      : Network.AWS.MarketplaceMetering.ResolveCustomer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ResolveCustomer is called by a SaaS application during the registration process. When a buyer visits your website during the registration process, the buyer submits a registration token through their browser. The registration token is resolved through this API to obtain a CustomerIdentifier and product code.
--
--
module Network.AWS.MarketplaceMetering.ResolveCustomer
    (
    -- * Creating a Request
      resolveCustomer
    , ResolveCustomer
    -- * Request Lenses
    , rcRegistrationToken

    -- * Destructuring the Response
    , resolveCustomerResponse
    , ResolveCustomerResponse
    -- * Response Lenses
    , rcrsCustomerIdentifier
    , rcrsProductCode
    , rcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MarketplaceMetering.Types
import Network.AWS.MarketplaceMetering.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains input to the ResolveCustomer operation.
--
--
--
-- /See:/ 'resolveCustomer' smart constructor.
newtype ResolveCustomer = ResolveCustomer'
  { _rcRegistrationToken :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveCustomer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRegistrationToken' - When a buyer visits your website during the registration process, the buyer submits a registration token through the browser. The registration token is resolved to obtain a CustomerIdentifier and product code.
resolveCustomer
    :: Text -- ^ 'rcRegistrationToken'
    -> ResolveCustomer
resolveCustomer pRegistrationToken_ =
  ResolveCustomer' {_rcRegistrationToken = pRegistrationToken_}


-- | When a buyer visits your website during the registration process, the buyer submits a registration token through the browser. The registration token is resolved to obtain a CustomerIdentifier and product code.
rcRegistrationToken :: Lens' ResolveCustomer Text
rcRegistrationToken = lens _rcRegistrationToken (\ s a -> s{_rcRegistrationToken = a})

instance AWSRequest ResolveCustomer where
        type Rs ResolveCustomer = ResolveCustomerResponse
        request = postJSON marketplaceMetering
        response
          = receiveJSON
              (\ s h x ->
                 ResolveCustomerResponse' <$>
                   (x .?> "CustomerIdentifier") <*>
                     (x .?> "ProductCode")
                     <*> (pure (fromEnum s)))

instance Hashable ResolveCustomer where

instance NFData ResolveCustomer where

instance ToHeaders ResolveCustomer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMPMeteringService.ResolveCustomer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResolveCustomer where
        toJSON ResolveCustomer'{..}
          = object
              (catMaybes
                 [Just ("RegistrationToken" .= _rcRegistrationToken)])

instance ToPath ResolveCustomer where
        toPath = const "/"

instance ToQuery ResolveCustomer where
        toQuery = const mempty

-- | The result of the ResolveCustomer operation. Contains the CustomerIdentifier and product code.
--
--
--
-- /See:/ 'resolveCustomerResponse' smart constructor.
data ResolveCustomerResponse = ResolveCustomerResponse'
  { _rcrsCustomerIdentifier :: !(Maybe Text)
  , _rcrsProductCode        :: !(Maybe Text)
  , _rcrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResolveCustomerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsCustomerIdentifier' - The CustomerIdentifier is used to identify an individual customer in your application. Calls to BatchMeterUsage require CustomerIdentifiers for each UsageRecord.
--
-- * 'rcrsProductCode' - The product code is returned to confirm that the buyer is registering for your product. Subsequent BatchMeterUsage calls should be made using this product code.
--
-- * 'rcrsResponseStatus' - -- | The response status code.
resolveCustomerResponse
    :: Int -- ^ 'rcrsResponseStatus'
    -> ResolveCustomerResponse
resolveCustomerResponse pResponseStatus_ =
  ResolveCustomerResponse'
    { _rcrsCustomerIdentifier = Nothing
    , _rcrsProductCode = Nothing
    , _rcrsResponseStatus = pResponseStatus_
    }


-- | The CustomerIdentifier is used to identify an individual customer in your application. Calls to BatchMeterUsage require CustomerIdentifiers for each UsageRecord.
rcrsCustomerIdentifier :: Lens' ResolveCustomerResponse (Maybe Text)
rcrsCustomerIdentifier = lens _rcrsCustomerIdentifier (\ s a -> s{_rcrsCustomerIdentifier = a})

-- | The product code is returned to confirm that the buyer is registering for your product. Subsequent BatchMeterUsage calls should be made using this product code.
rcrsProductCode :: Lens' ResolveCustomerResponse (Maybe Text)
rcrsProductCode = lens _rcrsProductCode (\ s a -> s{_rcrsProductCode = a})

-- | -- | The response status code.
rcrsResponseStatus :: Lens' ResolveCustomerResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a})

instance NFData ResolveCustomerResponse where
