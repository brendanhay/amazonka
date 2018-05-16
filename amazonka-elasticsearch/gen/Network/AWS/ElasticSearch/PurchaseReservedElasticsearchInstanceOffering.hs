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
-- Module      : Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to purchase reserved Elasticsearch instances.
--
--
module Network.AWS.ElasticSearch.PurchaseReservedElasticsearchInstanceOffering
    (
    -- * Creating a Request
      purchaseReservedElasticsearchInstanceOffering
    , PurchaseReservedElasticsearchInstanceOffering
    -- * Request Lenses
    , preioInstanceCount
    , preioReservedElasticsearchInstanceOfferingId
    , preioReservationName

    -- * Destructuring the Response
    , purchaseReservedElasticsearchInstanceOfferingResponse
    , PurchaseReservedElasticsearchInstanceOfferingResponse
    -- * Response Lenses
    , preiorsReservedElasticsearchInstanceId
    , preiorsReservationName
    , preiorsResponseStatus
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for parameters to @PurchaseReservedElasticsearchInstanceOffering@
--
--
--
-- /See:/ 'purchaseReservedElasticsearchInstanceOffering' smart constructor.
data PurchaseReservedElasticsearchInstanceOffering = PurchaseReservedElasticsearchInstanceOffering'
  { _preioInstanceCount                           :: !(Maybe Nat)
  , _preioReservedElasticsearchInstanceOfferingId :: !Text
  , _preioReservationName                         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseReservedElasticsearchInstanceOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'preioInstanceCount' - The number of Elasticsearch instances to reserve.
--
-- * 'preioReservedElasticsearchInstanceOfferingId' - The ID of the reserved Elasticsearch instance offering to purchase.
--
-- * 'preioReservationName' - A customer-specified identifier to track this reservation.
purchaseReservedElasticsearchInstanceOffering
    :: Text -- ^ 'preioReservedElasticsearchInstanceOfferingId'
    -> Text -- ^ 'preioReservationName'
    -> PurchaseReservedElasticsearchInstanceOffering
purchaseReservedElasticsearchInstanceOffering pReservedElasticsearchInstanceOfferingId_ pReservationName_ =
  PurchaseReservedElasticsearchInstanceOffering'
    { _preioInstanceCount = Nothing
    , _preioReservedElasticsearchInstanceOfferingId =
        pReservedElasticsearchInstanceOfferingId_
    , _preioReservationName = pReservationName_
    }


-- | The number of Elasticsearch instances to reserve.
preioInstanceCount :: Lens' PurchaseReservedElasticsearchInstanceOffering (Maybe Natural)
preioInstanceCount = lens _preioInstanceCount (\ s a -> s{_preioInstanceCount = a}) . mapping _Nat

-- | The ID of the reserved Elasticsearch instance offering to purchase.
preioReservedElasticsearchInstanceOfferingId :: Lens' PurchaseReservedElasticsearchInstanceOffering Text
preioReservedElasticsearchInstanceOfferingId = lens _preioReservedElasticsearchInstanceOfferingId (\ s a -> s{_preioReservedElasticsearchInstanceOfferingId = a})

-- | A customer-specified identifier to track this reservation.
preioReservationName :: Lens' PurchaseReservedElasticsearchInstanceOffering Text
preioReservationName = lens _preioReservationName (\ s a -> s{_preioReservationName = a})

instance AWSRequest
           PurchaseReservedElasticsearchInstanceOffering
         where
        type Rs PurchaseReservedElasticsearchInstanceOffering
             =
             PurchaseReservedElasticsearchInstanceOfferingResponse
        request = postJSON elasticSearch
        response
          = receiveJSON
              (\ s h x ->
                 PurchaseReservedElasticsearchInstanceOfferingResponse'
                   <$>
                   (x .?> "ReservedElasticsearchInstanceId") <*>
                     (x .?> "ReservationName")
                     <*> (pure (fromEnum s)))

instance Hashable
           PurchaseReservedElasticsearchInstanceOffering
         where

instance NFData
           PurchaseReservedElasticsearchInstanceOffering
         where

instance ToHeaders
           PurchaseReservedElasticsearchInstanceOffering
         where
        toHeaders = const mempty

instance ToJSON
           PurchaseReservedElasticsearchInstanceOffering
         where
        toJSON
          PurchaseReservedElasticsearchInstanceOffering'{..}
          = object
              (catMaybes
                 [("InstanceCount" .=) <$> _preioInstanceCount,
                  Just
                    ("ReservedElasticsearchInstanceOfferingId" .=
                       _preioReservedElasticsearchInstanceOfferingId),
                  Just ("ReservationName" .= _preioReservationName)])

instance ToPath
           PurchaseReservedElasticsearchInstanceOffering
         where
        toPath
          = const
              "/2015-01-01/es/purchaseReservedInstanceOffering"

instance ToQuery
           PurchaseReservedElasticsearchInstanceOffering
         where
        toQuery = const mempty

-- | Represents the output of a @PurchaseReservedElasticsearchInstanceOffering@ operation.
--
--
--
-- /See:/ 'purchaseReservedElasticsearchInstanceOfferingResponse' smart constructor.
data PurchaseReservedElasticsearchInstanceOfferingResponse = PurchaseReservedElasticsearchInstanceOfferingResponse'
  { _preiorsReservedElasticsearchInstanceId :: !(Maybe Text)
  , _preiorsReservationName                 :: !(Maybe Text)
  , _preiorsResponseStatus                  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PurchaseReservedElasticsearchInstanceOfferingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'preiorsReservedElasticsearchInstanceId' - Details of the reserved Elasticsearch instance which was purchased.
--
-- * 'preiorsReservationName' - The customer-specified identifier used to track this reservation.
--
-- * 'preiorsResponseStatus' - -- | The response status code.
purchaseReservedElasticsearchInstanceOfferingResponse
    :: Int -- ^ 'preiorsResponseStatus'
    -> PurchaseReservedElasticsearchInstanceOfferingResponse
purchaseReservedElasticsearchInstanceOfferingResponse pResponseStatus_ =
  PurchaseReservedElasticsearchInstanceOfferingResponse'
    { _preiorsReservedElasticsearchInstanceId = Nothing
    , _preiorsReservationName = Nothing
    , _preiorsResponseStatus = pResponseStatus_
    }


-- | Details of the reserved Elasticsearch instance which was purchased.
preiorsReservedElasticsearchInstanceId :: Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Maybe Text)
preiorsReservedElasticsearchInstanceId = lens _preiorsReservedElasticsearchInstanceId (\ s a -> s{_preiorsReservedElasticsearchInstanceId = a})

-- | The customer-specified identifier used to track this reservation.
preiorsReservationName :: Lens' PurchaseReservedElasticsearchInstanceOfferingResponse (Maybe Text)
preiorsReservationName = lens _preiorsReservationName (\ s a -> s{_preiorsReservationName = a})

-- | -- | The response status code.
preiorsResponseStatus :: Lens' PurchaseReservedElasticsearchInstanceOfferingResponse Int
preiorsResponseStatus = lens _preiorsResponseStatus (\ s a -> s{_preiorsResponseStatus = a})

instance NFData
           PurchaseReservedElasticsearchInstanceOfferingResponse
         where
