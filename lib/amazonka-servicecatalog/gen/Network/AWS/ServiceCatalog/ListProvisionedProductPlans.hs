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
-- Module      : Network.AWS.ServiceCatalog.ListProvisionedProductPlans
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the plans for the specified provisioned product or all plans to which the user has access.
--
--
module Network.AWS.ServiceCatalog.ListProvisionedProductPlans
    (
    -- * Creating a Request
      listProvisionedProductPlans
    , ListProvisionedProductPlans
    -- * Request Lenses
    , lpppProvisionProductId
    , lpppAcceptLanguage
    , lpppAccessLevelFilter
    , lpppPageToken
    , lpppPageSize

    -- * Destructuring the Response
    , listProvisionedProductPlansResponse
    , ListProvisionedProductPlansResponse
    -- * Response Lenses
    , lppprsNextPageToken
    , lppprsProvisionedProductPlans
    , lppprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'listProvisionedProductPlans' smart constructor.
data ListProvisionedProductPlans = ListProvisionedProductPlans'
  { _lpppProvisionProductId :: !(Maybe Text)
  , _lpppAcceptLanguage     :: !(Maybe Text)
  , _lpppAccessLevelFilter  :: !(Maybe AccessLevelFilter)
  , _lpppPageToken          :: !(Maybe Text)
  , _lpppPageSize           :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProvisionedProductPlans' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpppProvisionProductId' - The product identifier.
--
-- * 'lpppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lpppAccessLevelFilter' - The access level to use to obtain results. The default is @User@ .
--
-- * 'lpppPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lpppPageSize' - The maximum number of items to return with this call.
listProvisionedProductPlans
    :: ListProvisionedProductPlans
listProvisionedProductPlans =
  ListProvisionedProductPlans'
    { _lpppProvisionProductId = Nothing
    , _lpppAcceptLanguage = Nothing
    , _lpppAccessLevelFilter = Nothing
    , _lpppPageToken = Nothing
    , _lpppPageSize = Nothing
    }


-- | The product identifier.
lpppProvisionProductId :: Lens' ListProvisionedProductPlans (Maybe Text)
lpppProvisionProductId = lens _lpppProvisionProductId (\ s a -> s{_lpppProvisionProductId = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lpppAcceptLanguage :: Lens' ListProvisionedProductPlans (Maybe Text)
lpppAcceptLanguage = lens _lpppAcceptLanguage (\ s a -> s{_lpppAcceptLanguage = a})

-- | The access level to use to obtain results. The default is @User@ .
lpppAccessLevelFilter :: Lens' ListProvisionedProductPlans (Maybe AccessLevelFilter)
lpppAccessLevelFilter = lens _lpppAccessLevelFilter (\ s a -> s{_lpppAccessLevelFilter = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lpppPageToken :: Lens' ListProvisionedProductPlans (Maybe Text)
lpppPageToken = lens _lpppPageToken (\ s a -> s{_lpppPageToken = a})

-- | The maximum number of items to return with this call.
lpppPageSize :: Lens' ListProvisionedProductPlans (Maybe Natural)
lpppPageSize = lens _lpppPageSize (\ s a -> s{_lpppPageSize = a}) . mapping _Nat

instance AWSRequest ListProvisionedProductPlans where
        type Rs ListProvisionedProductPlans =
             ListProvisionedProductPlansResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 ListProvisionedProductPlansResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProvisionedProductPlans" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListProvisionedProductPlans where

instance NFData ListProvisionedProductPlans where

instance ToHeaders ListProvisionedProductPlans where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.ListProvisionedProductPlans"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListProvisionedProductPlans where
        toJSON ListProvisionedProductPlans'{..}
          = object
              (catMaybes
                 [("ProvisionProductId" .=) <$>
                    _lpppProvisionProductId,
                  ("AcceptLanguage" .=) <$> _lpppAcceptLanguage,
                  ("AccessLevelFilter" .=) <$> _lpppAccessLevelFilter,
                  ("PageToken" .=) <$> _lpppPageToken,
                  ("PageSize" .=) <$> _lpppPageSize])

instance ToPath ListProvisionedProductPlans where
        toPath = const "/"

instance ToQuery ListProvisionedProductPlans where
        toQuery = const mempty

-- | /See:/ 'listProvisionedProductPlansResponse' smart constructor.
data ListProvisionedProductPlansResponse = ListProvisionedProductPlansResponse'
  { _lppprsNextPageToken           :: !(Maybe Text)
  , _lppprsProvisionedProductPlans :: !(Maybe [ProvisionedProductPlanSummary])
  , _lppprsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListProvisionedProductPlansResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lppprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lppprsProvisionedProductPlans' - Information about the plans.
--
-- * 'lppprsResponseStatus' - -- | The response status code.
listProvisionedProductPlansResponse
    :: Int -- ^ 'lppprsResponseStatus'
    -> ListProvisionedProductPlansResponse
listProvisionedProductPlansResponse pResponseStatus_ =
  ListProvisionedProductPlansResponse'
    { _lppprsNextPageToken = Nothing
    , _lppprsProvisionedProductPlans = Nothing
    , _lppprsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lppprsNextPageToken :: Lens' ListProvisionedProductPlansResponse (Maybe Text)
lppprsNextPageToken = lens _lppprsNextPageToken (\ s a -> s{_lppprsNextPageToken = a})

-- | Information about the plans.
lppprsProvisionedProductPlans :: Lens' ListProvisionedProductPlansResponse [ProvisionedProductPlanSummary]
lppprsProvisionedProductPlans = lens _lppprsProvisionedProductPlans (\ s a -> s{_lppprsProvisionedProductPlans = a}) . _Default . _Coerce

-- | -- | The response status code.
lppprsResponseStatus :: Lens' ListProvisionedProductPlansResponse Int
lppprsResponseStatus = lens _lppprsResponseStatus (\ s a -> s{_lppprsResponseStatus = a})

instance NFData ListProvisionedProductPlansResponse
         where
