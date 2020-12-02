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
-- Module      : Network.AWS.Redshift.PurchaseReservedNodeOffering
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved nodes. Amazon Redshift offers a predefined set of reserved node offerings. You can purchase one or more of the offerings. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings. You can call this API by providing a specific reserved node offering and the number of nodes you want to reserve.
--
--
-- For more information about reserved node offerings, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/purchase-reserved-node-instance.html Purchasing Reserved Nodes> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.PurchaseReservedNodeOffering
    (
    -- * Creating a Request
      purchaseReservedNodeOffering
    , PurchaseReservedNodeOffering
    -- * Request Lenses
    , prnoNodeCount
    , prnoReservedNodeOfferingId

    -- * Destructuring the Response
    , purchaseReservedNodeOfferingResponse
    , PurchaseReservedNodeOfferingResponse
    -- * Response Lenses
    , prnorsReservedNode
    , prnorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'purchaseReservedNodeOffering' smart constructor.
data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering'
  { _prnoNodeCount              :: !(Maybe Int)
  , _prnoReservedNodeOfferingId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseReservedNodeOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prnoNodeCount' - The number of reserved nodes that you want to purchase. Default: @1@
--
-- * 'prnoReservedNodeOfferingId' - The unique identifier of the reserved node offering you want to purchase.
purchaseReservedNodeOffering
    :: Text -- ^ 'prnoReservedNodeOfferingId'
    -> PurchaseReservedNodeOffering
purchaseReservedNodeOffering pReservedNodeOfferingId_ =
  PurchaseReservedNodeOffering'
    { _prnoNodeCount = Nothing
    , _prnoReservedNodeOfferingId = pReservedNodeOfferingId_
    }


-- | The number of reserved nodes that you want to purchase. Default: @1@
prnoNodeCount :: Lens' PurchaseReservedNodeOffering (Maybe Int)
prnoNodeCount = lens _prnoNodeCount (\ s a -> s{_prnoNodeCount = a})

-- | The unique identifier of the reserved node offering you want to purchase.
prnoReservedNodeOfferingId :: Lens' PurchaseReservedNodeOffering Text
prnoReservedNodeOfferingId = lens _prnoReservedNodeOfferingId (\ s a -> s{_prnoReservedNodeOfferingId = a})

instance AWSRequest PurchaseReservedNodeOffering
         where
        type Rs PurchaseReservedNodeOffering =
             PurchaseReservedNodeOfferingResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "PurchaseReservedNodeOfferingResult"
              (\ s h x ->
                 PurchaseReservedNodeOfferingResponse' <$>
                   (x .@? "ReservedNode") <*> (pure (fromEnum s)))

instance Hashable PurchaseReservedNodeOffering where

instance NFData PurchaseReservedNodeOffering where

instance ToHeaders PurchaseReservedNodeOffering where
        toHeaders = const mempty

instance ToPath PurchaseReservedNodeOffering where
        toPath = const "/"

instance ToQuery PurchaseReservedNodeOffering where
        toQuery PurchaseReservedNodeOffering'{..}
          = mconcat
              ["Action" =:
                 ("PurchaseReservedNodeOffering" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "NodeCount" =: _prnoNodeCount,
               "ReservedNodeOfferingId" =:
                 _prnoReservedNodeOfferingId]

-- | /See:/ 'purchaseReservedNodeOfferingResponse' smart constructor.
data PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse'
  { _prnorsReservedNode   :: !(Maybe ReservedNode)
  , _prnorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseReservedNodeOfferingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prnorsReservedNode' - Undocumented member.
--
-- * 'prnorsResponseStatus' - -- | The response status code.
purchaseReservedNodeOfferingResponse
    :: Int -- ^ 'prnorsResponseStatus'
    -> PurchaseReservedNodeOfferingResponse
purchaseReservedNodeOfferingResponse pResponseStatus_ =
  PurchaseReservedNodeOfferingResponse'
    {_prnorsReservedNode = Nothing, _prnorsResponseStatus = pResponseStatus_}


-- | Undocumented member.
prnorsReservedNode :: Lens' PurchaseReservedNodeOfferingResponse (Maybe ReservedNode)
prnorsReservedNode = lens _prnorsReservedNode (\ s a -> s{_prnorsReservedNode = a})

-- | -- | The response status code.
prnorsResponseStatus :: Lens' PurchaseReservedNodeOfferingResponse Int
prnorsResponseStatus = lens _prnorsResponseStatus (\ s a -> s{_prnorsResponseStatus = a})

instance NFData PurchaseReservedNodeOfferingResponse
         where
