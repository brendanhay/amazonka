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
-- Module      : Network.AWS.ServiceCatalog.DescribeProduct
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified product.
--
-- This operation is functionally identical to < DescribeProductView> except that it takes as input 'ProductId' instead of 'ProductViewId'.
module Network.AWS.ServiceCatalog.DescribeProduct
    (
    -- * Creating a Request
      describeProduct
    , DescribeProduct
    -- * Request Lenses
    , dpAcceptLanguage
    , dpId

    -- * Destructuring the Response
    , describeProductResponse
    , DescribeProductResponse
    -- * Response Lenses
    , dprsProductViewSummary
    , dprsProvisioningArtifacts
    , dprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProduct' smart constructor.
data DescribeProduct = DescribeProduct'
    { _dpAcceptLanguage :: !(Maybe Text)
    , _dpId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpAcceptLanguage'
--
-- * 'dpId'
describeProduct
    :: Text -- ^ 'dpId'
    -> DescribeProduct
describeProduct pId_ =
    DescribeProduct'
    { _dpAcceptLanguage = Nothing
    , _dpId = pId_
    }

-- | Optional language code. Supported language codes are as follows:
--
-- \"en\" (English)
--
-- \"jp\" (Japanese)
--
-- \"zh\" (Chinese)
--
-- If no code is specified, \"en\" is used as the default.
dpAcceptLanguage :: Lens' DescribeProduct (Maybe Text)
dpAcceptLanguage = lens _dpAcceptLanguage (\ s a -> s{_dpAcceptLanguage = a});

-- | The 'ProductId' of the product to describe.
dpId :: Lens' DescribeProduct Text
dpId = lens _dpId (\ s a -> s{_dpId = a});

instance AWSRequest DescribeProduct where
        type Rs DescribeProduct = DescribeProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProductResponse' <$>
                   (x .?> "ProductViewSummary") <*>
                     (x .?> "ProvisioningArtifacts" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProduct

instance NFData DescribeProduct

instance ToHeaders DescribeProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProduct" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProduct where
        toJSON DescribeProduct'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpAcceptLanguage,
                  Just ("Id" .= _dpId)])

instance ToPath DescribeProduct where
        toPath = const "/"

instance ToQuery DescribeProduct where
        toQuery = const mempty

-- | /See:/ 'describeProductResponse' smart constructor.
data DescribeProductResponse = DescribeProductResponse'
    { _dprsProductViewSummary    :: !(Maybe ProductViewSummary)
    , _dprsProvisioningArtifacts :: !(Maybe [ProvisioningArtifact])
    , _dprsResponseStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsProductViewSummary'
--
-- * 'dprsProvisioningArtifacts'
--
-- * 'dprsResponseStatus'
describeProductResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribeProductResponse
describeProductResponse pResponseStatus_ =
    DescribeProductResponse'
    { _dprsProductViewSummary = Nothing
    , _dprsProvisioningArtifacts = Nothing
    , _dprsResponseStatus = pResponseStatus_
    }

-- | The summary metadata about the specified product.
dprsProductViewSummary :: Lens' DescribeProductResponse (Maybe ProductViewSummary)
dprsProductViewSummary = lens _dprsProductViewSummary (\ s a -> s{_dprsProductViewSummary = a});

-- | A list of provisioning artifact objects for the specified product. The 'ProvisioningArtifacts' parameter represent the ways the specified product can be provisioned.
dprsProvisioningArtifacts :: Lens' DescribeProductResponse [ProvisioningArtifact]
dprsProvisioningArtifacts = lens _dprsProvisioningArtifacts (\ s a -> s{_dprsProvisioningArtifacts = a}) . _Default . _Coerce;

-- | The response status code.
dprsResponseStatus :: Lens' DescribeProductResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a});

instance NFData DescribeProductResponse
