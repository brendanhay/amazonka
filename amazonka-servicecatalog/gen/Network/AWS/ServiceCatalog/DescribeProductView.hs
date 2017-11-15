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
-- Module      : Network.AWS.ServiceCatalog.DescribeProductView
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified product.
--
--
-- This operation is functionally identical to 'DescribeProduct' except that it takes as input @ProductViewId@ instead of @ProductId@ .
--
module Network.AWS.ServiceCatalog.DescribeProductView
    (
    -- * Creating a Request
      describeProductView
    , DescribeProductView
    -- * Request Lenses
    , dpvAcceptLanguage
    , dpvId

    -- * Destructuring the Response
    , describeProductViewResponse
    , DescribeProductViewResponse
    -- * Response Lenses
    , dpvrsProductViewSummary
    , dpvrsProvisioningArtifacts
    , dpvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProductView' smart constructor.
data DescribeProductView = DescribeProductView'
  { _dpvAcceptLanguage :: !(Maybe Text)
  , _dpvId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProductView' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpvId' - The @ProductViewId@ of the product to describe.
describeProductView
    :: Text -- ^ 'dpvId'
    -> DescribeProductView
describeProductView pId_ =
  DescribeProductView' {_dpvAcceptLanguage = Nothing, _dpvId = pId_}


-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpvAcceptLanguage :: Lens' DescribeProductView (Maybe Text)
dpvAcceptLanguage = lens _dpvAcceptLanguage (\ s a -> s{_dpvAcceptLanguage = a});

-- | The @ProductViewId@ of the product to describe.
dpvId :: Lens' DescribeProductView Text
dpvId = lens _dpvId (\ s a -> s{_dpvId = a});

instance AWSRequest DescribeProductView where
        type Rs DescribeProductView =
             DescribeProductViewResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProductViewResponse' <$>
                   (x .?> "ProductViewSummary") <*>
                     (x .?> "ProvisioningArtifacts" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProductView where

instance NFData DescribeProductView where

instance ToHeaders DescribeProductView where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProductView" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProductView where
        toJSON DescribeProductView'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpvAcceptLanguage,
                  Just ("Id" .= _dpvId)])

instance ToPath DescribeProductView where
        toPath = const "/"

instance ToQuery DescribeProductView where
        toQuery = const mempty

-- | /See:/ 'describeProductViewResponse' smart constructor.
data DescribeProductViewResponse = DescribeProductViewResponse'
  { _dpvrsProductViewSummary    :: !(Maybe ProductViewSummary)
  , _dpvrsProvisioningArtifacts :: !(Maybe [ProvisioningArtifact])
  , _dpvrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProductViewResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvrsProductViewSummary' - The summary metadata about the specified product.
--
-- * 'dpvrsProvisioningArtifacts' - A list of provisioning artifact objects for the specified product. The @ProvisioningArtifacts@ represent the ways in which the specified product can be provisioned.
--
-- * 'dpvrsResponseStatus' - -- | The response status code.
describeProductViewResponse
    :: Int -- ^ 'dpvrsResponseStatus'
    -> DescribeProductViewResponse
describeProductViewResponse pResponseStatus_ =
  DescribeProductViewResponse'
  { _dpvrsProductViewSummary = Nothing
  , _dpvrsProvisioningArtifacts = Nothing
  , _dpvrsResponseStatus = pResponseStatus_
  }


-- | The summary metadata about the specified product.
dpvrsProductViewSummary :: Lens' DescribeProductViewResponse (Maybe ProductViewSummary)
dpvrsProductViewSummary = lens _dpvrsProductViewSummary (\ s a -> s{_dpvrsProductViewSummary = a});

-- | A list of provisioning artifact objects for the specified product. The @ProvisioningArtifacts@ represent the ways in which the specified product can be provisioned.
dpvrsProvisioningArtifacts :: Lens' DescribeProductViewResponse [ProvisioningArtifact]
dpvrsProvisioningArtifacts = lens _dpvrsProvisioningArtifacts (\ s a -> s{_dpvrsProvisioningArtifacts = a}) . _Default . _Coerce;

-- | -- | The response status code.
dpvrsResponseStatus :: Lens' DescribeProductViewResponse Int
dpvrsResponseStatus = lens _dpvrsResponseStatus (\ s a -> s{_dpvrsResponseStatus = a});

instance NFData DescribeProductViewResponse where
