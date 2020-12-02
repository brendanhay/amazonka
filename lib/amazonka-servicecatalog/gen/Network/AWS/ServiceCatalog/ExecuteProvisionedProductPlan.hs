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
-- Module      : Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provisions or modifies a product based on the resource changes for the specified plan.
--
--
module Network.AWS.ServiceCatalog.ExecuteProvisionedProductPlan
    (
    -- * Creating a Request
      executeProvisionedProductPlan
    , ExecuteProvisionedProductPlan
    -- * Request Lenses
    , epppAcceptLanguage
    , epppPlanId
    , epppIdempotencyToken

    -- * Destructuring the Response
    , executeProvisionedProductPlanResponse
    , ExecuteProvisionedProductPlanResponse
    -- * Response Lenses
    , eppprsRecordDetail
    , eppprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'executeProvisionedProductPlan' smart constructor.
data ExecuteProvisionedProductPlan = ExecuteProvisionedProductPlan'
  { _epppAcceptLanguage   :: !(Maybe Text)
  , _epppPlanId           :: !Text
  , _epppIdempotencyToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecuteProvisionedProductPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'epppPlanId' - The plan identifier.
--
-- * 'epppIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
executeProvisionedProductPlan
    :: Text -- ^ 'epppPlanId'
    -> Text -- ^ 'epppIdempotencyToken'
    -> ExecuteProvisionedProductPlan
executeProvisionedProductPlan pPlanId_ pIdempotencyToken_ =
  ExecuteProvisionedProductPlan'
    { _epppAcceptLanguage = Nothing
    , _epppPlanId = pPlanId_
    , _epppIdempotencyToken = pIdempotencyToken_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
epppAcceptLanguage :: Lens' ExecuteProvisionedProductPlan (Maybe Text)
epppAcceptLanguage = lens _epppAcceptLanguage (\ s a -> s{_epppAcceptLanguage = a})

-- | The plan identifier.
epppPlanId :: Lens' ExecuteProvisionedProductPlan Text
epppPlanId = lens _epppPlanId (\ s a -> s{_epppPlanId = a})

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
epppIdempotencyToken :: Lens' ExecuteProvisionedProductPlan Text
epppIdempotencyToken = lens _epppIdempotencyToken (\ s a -> s{_epppIdempotencyToken = a})

instance AWSRequest ExecuteProvisionedProductPlan
         where
        type Rs ExecuteProvisionedProductPlan =
             ExecuteProvisionedProductPlanResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ExecuteProvisionedProductPlanResponse' <$>
                   (x .?> "RecordDetail") <*> (pure (fromEnum s)))

instance Hashable ExecuteProvisionedProductPlan where

instance NFData ExecuteProvisionedProductPlan where

instance ToHeaders ExecuteProvisionedProductPlan
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ExecuteProvisionedProductPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ExecuteProvisionedProductPlan where
        toJSON ExecuteProvisionedProductPlan'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _epppAcceptLanguage,
                  Just ("PlanId" .= _epppPlanId),
                  Just ("IdempotencyToken" .= _epppIdempotencyToken)])

instance ToPath ExecuteProvisionedProductPlan where
        toPath = const "/"

instance ToQuery ExecuteProvisionedProductPlan where
        toQuery = const mempty

-- | /See:/ 'executeProvisionedProductPlanResponse' smart constructor.
data ExecuteProvisionedProductPlanResponse = ExecuteProvisionedProductPlanResponse'
  { _eppprsRecordDetail   :: !(Maybe RecordDetail)
  , _eppprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExecuteProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eppprsRecordDetail' - Information about the result of provisioning the product.
--
-- * 'eppprsResponseStatus' - -- | The response status code.
executeProvisionedProductPlanResponse
    :: Int -- ^ 'eppprsResponseStatus'
    -> ExecuteProvisionedProductPlanResponse
executeProvisionedProductPlanResponse pResponseStatus_ =
  ExecuteProvisionedProductPlanResponse'
    {_eppprsRecordDetail = Nothing, _eppprsResponseStatus = pResponseStatus_}


-- | Information about the result of provisioning the product.
eppprsRecordDetail :: Lens' ExecuteProvisionedProductPlanResponse (Maybe RecordDetail)
eppprsRecordDetail = lens _eppprsRecordDetail (\ s a -> s{_eppprsRecordDetail = a})

-- | -- | The response status code.
eppprsResponseStatus :: Lens' ExecuteProvisionedProductPlanResponse Int
eppprsResponseStatus = lens _eppprsResponseStatus (\ s a -> s{_eppprsResponseStatus = a})

instance NFData ExecuteProvisionedProductPlanResponse
         where
