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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the resource changes for the specified plan.
--
--
module Network.AWS.ServiceCatalog.DescribeProvisionedProductPlan
    (
    -- * Creating a Request
      describeProvisionedProductPlan
    , DescribeProvisionedProductPlan
    -- * Request Lenses
    , dpppAcceptLanguage
    , dpppPageToken
    , dpppPageSize
    , dpppPlanId

    -- * Destructuring the Response
    , describeProvisionedProductPlanResponse
    , DescribeProvisionedProductPlanResponse
    -- * Response Lenses
    , dpppprsNextPageToken
    , dpppprsProvisionedProductPlanDetails
    , dpppprsResourceChanges
    , dpppprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProvisionedProductPlan' smart constructor.
data DescribeProvisionedProductPlan = DescribeProvisionedProductPlan'
  { _dpppAcceptLanguage :: !(Maybe Text)
  , _dpppPageToken      :: !(Maybe Text)
  , _dpppPageSize       :: !(Maybe Nat)
  , _dpppPlanId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProvisionedProductPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpppAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpppPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'dpppPageSize' - The maximum number of items to return with this call.
--
-- * 'dpppPlanId' - The plan identifier.
describeProvisionedProductPlan
    :: Text -- ^ 'dpppPlanId'
    -> DescribeProvisionedProductPlan
describeProvisionedProductPlan pPlanId_ =
  DescribeProvisionedProductPlan'
    { _dpppAcceptLanguage = Nothing
    , _dpppPageToken = Nothing
    , _dpppPageSize = Nothing
    , _dpppPlanId = pPlanId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpppAcceptLanguage :: Lens' DescribeProvisionedProductPlan (Maybe Text)
dpppAcceptLanguage = lens _dpppAcceptLanguage (\ s a -> s{_dpppAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
dpppPageToken :: Lens' DescribeProvisionedProductPlan (Maybe Text)
dpppPageToken = lens _dpppPageToken (\ s a -> s{_dpppPageToken = a})

-- | The maximum number of items to return with this call.
dpppPageSize :: Lens' DescribeProvisionedProductPlan (Maybe Natural)
dpppPageSize = lens _dpppPageSize (\ s a -> s{_dpppPageSize = a}) . mapping _Nat

-- | The plan identifier.
dpppPlanId :: Lens' DescribeProvisionedProductPlan Text
dpppPlanId = lens _dpppPlanId (\ s a -> s{_dpppPlanId = a})

instance AWSRequest DescribeProvisionedProductPlan
         where
        type Rs DescribeProvisionedProductPlan =
             DescribeProvisionedProductPlanResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProvisionedProductPlanResponse' <$>
                   (x .?> "NextPageToken") <*>
                     (x .?> "ProvisionedProductPlanDetails")
                     <*> (x .?> "ResourceChanges" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProvisionedProductPlan
         where

instance NFData DescribeProvisionedProductPlan where

instance ToHeaders DescribeProvisionedProductPlan
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProvisionedProductPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProvisionedProductPlan where
        toJSON DescribeProvisionedProductPlan'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpppAcceptLanguage,
                  ("PageToken" .=) <$> _dpppPageToken,
                  ("PageSize" .=) <$> _dpppPageSize,
                  Just ("PlanId" .= _dpppPlanId)])

instance ToPath DescribeProvisionedProductPlan where
        toPath = const "/"

instance ToQuery DescribeProvisionedProductPlan where
        toQuery = const mempty

-- | /See:/ 'describeProvisionedProductPlanResponse' smart constructor.
data DescribeProvisionedProductPlanResponse = DescribeProvisionedProductPlanResponse'
  { _dpppprsNextPageToken :: !(Maybe Text)
  , _dpppprsProvisionedProductPlanDetails :: !(Maybe ProvisionedProductPlanDetails)
  , _dpppprsResourceChanges :: !(Maybe [ResourceChange])
  , _dpppprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpppprsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'dpppprsProvisionedProductPlanDetails' - Information about the plan.
--
-- * 'dpppprsResourceChanges' - Information about the resource changes that will occur when the plan is executed.
--
-- * 'dpppprsResponseStatus' - -- | The response status code.
describeProvisionedProductPlanResponse
    :: Int -- ^ 'dpppprsResponseStatus'
    -> DescribeProvisionedProductPlanResponse
describeProvisionedProductPlanResponse pResponseStatus_ =
  DescribeProvisionedProductPlanResponse'
    { _dpppprsNextPageToken = Nothing
    , _dpppprsProvisionedProductPlanDetails = Nothing
    , _dpppprsResourceChanges = Nothing
    , _dpppprsResponseStatus = pResponseStatus_
    }


-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
dpppprsNextPageToken :: Lens' DescribeProvisionedProductPlanResponse (Maybe Text)
dpppprsNextPageToken = lens _dpppprsNextPageToken (\ s a -> s{_dpppprsNextPageToken = a})

-- | Information about the plan.
dpppprsProvisionedProductPlanDetails :: Lens' DescribeProvisionedProductPlanResponse (Maybe ProvisionedProductPlanDetails)
dpppprsProvisionedProductPlanDetails = lens _dpppprsProvisionedProductPlanDetails (\ s a -> s{_dpppprsProvisionedProductPlanDetails = a})

-- | Information about the resource changes that will occur when the plan is executed.
dpppprsResourceChanges :: Lens' DescribeProvisionedProductPlanResponse [ResourceChange]
dpppprsResourceChanges = lens _dpppprsResourceChanges (\ s a -> s{_dpppprsResourceChanges = a}) . _Default . _Coerce

-- | -- | The response status code.
dpppprsResponseStatus :: Lens' DescribeProvisionedProductPlanResponse Int
dpppprsResponseStatus = lens _dpppprsResponseStatus (\ s a -> s{_dpppprsResponseStatus = a})

instance NFData
           DescribeProvisionedProductPlanResponse
         where
