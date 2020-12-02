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
-- Module      : Network.AWS.Glacier.PurchaseProvisionedCapacity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation purchases a provisioned capacity unit for an AWS account.
--
--
module Network.AWS.Glacier.PurchaseProvisionedCapacity
    (
    -- * Creating a Request
      purchaseProvisionedCapacity
    , PurchaseProvisionedCapacity
    -- * Request Lenses
    , ppcAccountId

    -- * Destructuring the Response
    , purchaseProvisionedCapacityResponse
    , PurchaseProvisionedCapacityResponse
    -- * Response Lenses
    , ppcrsCapacityId
    , ppcrsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'purchaseProvisionedCapacity' smart constructor.
newtype PurchaseProvisionedCapacity = PurchaseProvisionedCapacity'
  { _ppcAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseProvisionedCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppcAccountId' - The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
purchaseProvisionedCapacity
    :: Text -- ^ 'ppcAccountId'
    -> PurchaseProvisionedCapacity
purchaseProvisionedCapacity pAccountId_ =
  PurchaseProvisionedCapacity' {_ppcAccountId = pAccountId_}


-- | The AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '-' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, don't include any hyphens ('-') in the ID.
ppcAccountId :: Lens' PurchaseProvisionedCapacity Text
ppcAccountId = lens _ppcAccountId (\ s a -> s{_ppcAccountId = a})

instance AWSRequest PurchaseProvisionedCapacity where
        type Rs PurchaseProvisionedCapacity =
             PurchaseProvisionedCapacityResponse
        request = postJSON glacier
        response
          = receiveEmpty
              (\ s h x ->
                 PurchaseProvisionedCapacityResponse' <$>
                   (h .#? "x-amz-capacity-id") <*> (pure (fromEnum s)))

instance Hashable PurchaseProvisionedCapacity where

instance NFData PurchaseProvisionedCapacity where

instance ToHeaders PurchaseProvisionedCapacity where
        toHeaders = const mempty

instance ToJSON PurchaseProvisionedCapacity where
        toJSON = const (Object mempty)

instance ToPath PurchaseProvisionedCapacity where
        toPath PurchaseProvisionedCapacity'{..}
          = mconcat
              ["/", toBS _ppcAccountId, "/provisioned-capacity"]

instance ToQuery PurchaseProvisionedCapacity where
        toQuery = const mempty

-- | /See:/ 'purchaseProvisionedCapacityResponse' smart constructor.
data PurchaseProvisionedCapacityResponse = PurchaseProvisionedCapacityResponse'
  { _ppcrsCapacityId     :: !(Maybe Text)
  , _ppcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseProvisionedCapacityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppcrsCapacityId' - The ID that identifies the provisioned capacity unit.
--
-- * 'ppcrsResponseStatus' - -- | The response status code.
purchaseProvisionedCapacityResponse
    :: Int -- ^ 'ppcrsResponseStatus'
    -> PurchaseProvisionedCapacityResponse
purchaseProvisionedCapacityResponse pResponseStatus_ =
  PurchaseProvisionedCapacityResponse'
    {_ppcrsCapacityId = Nothing, _ppcrsResponseStatus = pResponseStatus_}


-- | The ID that identifies the provisioned capacity unit.
ppcrsCapacityId :: Lens' PurchaseProvisionedCapacityResponse (Maybe Text)
ppcrsCapacityId = lens _ppcrsCapacityId (\ s a -> s{_ppcrsCapacityId = a})

-- | -- | The response status code.
ppcrsResponseStatus :: Lens' PurchaseProvisionedCapacityResponse Int
ppcrsResponseStatus = lens _ppcrsResponseStatus (\ s a -> s{_ppcrsResponseStatus = a})

instance NFData PurchaseProvisionedCapacityResponse
         where
