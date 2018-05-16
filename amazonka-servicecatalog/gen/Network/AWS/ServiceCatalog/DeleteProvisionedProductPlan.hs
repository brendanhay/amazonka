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
-- Module      : Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified plan.
--
--
module Network.AWS.ServiceCatalog.DeleteProvisionedProductPlan
    (
    -- * Creating a Request
      deleteProvisionedProductPlan
    , DeleteProvisionedProductPlan
    -- * Request Lenses
    , delAcceptLanguage
    , delIgnoreErrors
    , delPlanId

    -- * Destructuring the Response
    , deleteProvisionedProductPlanResponse
    , DeleteProvisionedProductPlanResponse
    -- * Response Lenses
    , dppprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deleteProvisionedProductPlan' smart constructor.
data DeleteProvisionedProductPlan = DeleteProvisionedProductPlan'
  { _delAcceptLanguage :: !(Maybe Text)
  , _delIgnoreErrors   :: !(Maybe Bool)
  , _delPlanId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProvisionedProductPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'delIgnoreErrors' - If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
--
-- * 'delPlanId' - The plan identifier.
deleteProvisionedProductPlan
    :: Text -- ^ 'delPlanId'
    -> DeleteProvisionedProductPlan
deleteProvisionedProductPlan pPlanId_ =
  DeleteProvisionedProductPlan'
    { _delAcceptLanguage = Nothing
    , _delIgnoreErrors = Nothing
    , _delPlanId = pPlanId_
    }


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
delAcceptLanguage :: Lens' DeleteProvisionedProductPlan (Maybe Text)
delAcceptLanguage = lens _delAcceptLanguage (\ s a -> s{_delAcceptLanguage = a})

-- | If set to true, AWS Service Catalog stops managing the specified provisioned product even if it cannot delete the underlying resources.
delIgnoreErrors :: Lens' DeleteProvisionedProductPlan (Maybe Bool)
delIgnoreErrors = lens _delIgnoreErrors (\ s a -> s{_delIgnoreErrors = a})

-- | The plan identifier.
delPlanId :: Lens' DeleteProvisionedProductPlan Text
delPlanId = lens _delPlanId (\ s a -> s{_delPlanId = a})

instance AWSRequest DeleteProvisionedProductPlan
         where
        type Rs DeleteProvisionedProductPlan =
             DeleteProvisionedProductPlanResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProvisionedProductPlanResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteProvisionedProductPlan where

instance NFData DeleteProvisionedProductPlan where

instance ToHeaders DeleteProvisionedProductPlan where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeleteProvisionedProductPlan"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProvisionedProductPlan where
        toJSON DeleteProvisionedProductPlan'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _delAcceptLanguage,
                  ("IgnoreErrors" .=) <$> _delIgnoreErrors,
                  Just ("PlanId" .= _delPlanId)])

instance ToPath DeleteProvisionedProductPlan where
        toPath = const "/"

instance ToQuery DeleteProvisionedProductPlan where
        toQuery = const mempty

-- | /See:/ 'deleteProvisionedProductPlanResponse' smart constructor.
newtype DeleteProvisionedProductPlanResponse = DeleteProvisionedProductPlanResponse'
  { _dppprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProvisionedProductPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dppprsResponseStatus' - -- | The response status code.
deleteProvisionedProductPlanResponse
    :: Int -- ^ 'dppprsResponseStatus'
    -> DeleteProvisionedProductPlanResponse
deleteProvisionedProductPlanResponse pResponseStatus_ =
  DeleteProvisionedProductPlanResponse'
    {_dppprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dppprsResponseStatus :: Lens' DeleteProvisionedProductPlanResponse Int
dppprsResponseStatus = lens _dppprsResponseStatus (\ s a -> s{_dppprsResponseStatus = a})

instance NFData DeleteProvisionedProductPlanResponse
         where
