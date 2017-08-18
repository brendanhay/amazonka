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
-- Module      : Network.AWS.ServiceCatalog.DescribeProductAsAdmin
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified product, run with administrator access.
--
--
module Network.AWS.ServiceCatalog.DescribeProductAsAdmin
    (
    -- * Creating a Request
      describeProductAsAdmin
    , DescribeProductAsAdmin
    -- * Request Lenses
    , dpaaAcceptLanguage
    , dpaaId

    -- * Destructuring the Response
    , describeProductAsAdminResponse
    , DescribeProductAsAdminResponse
    -- * Response Lenses
    , dpaarsProductViewDetail
    , dpaarsTagOptions
    , dpaarsProvisioningArtifactSummaries
    , dpaarsTags
    , dpaarsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProductAsAdmin' smart constructor.
data DescribeProductAsAdmin = DescribeProductAsAdmin'
    { _dpaaAcceptLanguage :: !(Maybe Text)
    , _dpaaId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProductAsAdmin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpaaAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'dpaaId' - The identifier of the product for which to retrieve information.
describeProductAsAdmin
    :: Text -- ^ 'dpaaId'
    -> DescribeProductAsAdmin
describeProductAsAdmin pId_ =
    DescribeProductAsAdmin'
    { _dpaaAcceptLanguage = Nothing
    , _dpaaId = pId_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
dpaaAcceptLanguage :: Lens' DescribeProductAsAdmin (Maybe Text)
dpaaAcceptLanguage = lens _dpaaAcceptLanguage (\ s a -> s{_dpaaAcceptLanguage = a});

-- | The identifier of the product for which to retrieve information.
dpaaId :: Lens' DescribeProductAsAdmin Text
dpaaId = lens _dpaaId (\ s a -> s{_dpaaId = a});

instance AWSRequest DescribeProductAsAdmin where
        type Rs DescribeProductAsAdmin =
             DescribeProductAsAdminResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProductAsAdminResponse' <$>
                   (x .?> "ProductViewDetail") <*>
                     (x .?> "TagOptions" .!@ mempty)
                     <*>
                     (x .?> "ProvisioningArtifactSummaries" .!@ mempty)
                     <*> (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProductAsAdmin

instance NFData DescribeProductAsAdmin

instance ToHeaders DescribeProductAsAdmin where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProductAsAdmin"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProductAsAdmin where
        toJSON DescribeProductAsAdmin'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dpaaAcceptLanguage,
                  Just ("Id" .= _dpaaId)])

instance ToPath DescribeProductAsAdmin where
        toPath = const "/"

instance ToQuery DescribeProductAsAdmin where
        toQuery = const mempty

-- | /See:/ 'describeProductAsAdminResponse' smart constructor.
data DescribeProductAsAdminResponse = DescribeProductAsAdminResponse'
    { _dpaarsProductViewDetail             :: !(Maybe ProductViewDetail)
    , _dpaarsTagOptions                    :: !(Maybe [TagOptionDetail])
    , _dpaarsProvisioningArtifactSummaries :: !(Maybe [ProvisioningArtifactSummary])
    , _dpaarsTags                          :: !(Maybe [Tag])
    , _dpaarsResponseStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeProductAsAdminResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpaarsProductViewDetail' - Detailed product view information.
--
-- * 'dpaarsTagOptions' - List of TagOptions associated with the product.
--
-- * 'dpaarsProvisioningArtifactSummaries' - A list of provisioning artifact summaries for the product.
--
-- * 'dpaarsTags' - Tags associated with the product.
--
-- * 'dpaarsResponseStatus' - -- | The response status code.
describeProductAsAdminResponse
    :: Int -- ^ 'dpaarsResponseStatus'
    -> DescribeProductAsAdminResponse
describeProductAsAdminResponse pResponseStatus_ =
    DescribeProductAsAdminResponse'
    { _dpaarsProductViewDetail = Nothing
    , _dpaarsTagOptions = Nothing
    , _dpaarsProvisioningArtifactSummaries = Nothing
    , _dpaarsTags = Nothing
    , _dpaarsResponseStatus = pResponseStatus_
    }

-- | Detailed product view information.
dpaarsProductViewDetail :: Lens' DescribeProductAsAdminResponse (Maybe ProductViewDetail)
dpaarsProductViewDetail = lens _dpaarsProductViewDetail (\ s a -> s{_dpaarsProductViewDetail = a});

-- | List of TagOptions associated with the product.
dpaarsTagOptions :: Lens' DescribeProductAsAdminResponse [TagOptionDetail]
dpaarsTagOptions = lens _dpaarsTagOptions (\ s a -> s{_dpaarsTagOptions = a}) . _Default . _Coerce;

-- | A list of provisioning artifact summaries for the product.
dpaarsProvisioningArtifactSummaries :: Lens' DescribeProductAsAdminResponse [ProvisioningArtifactSummary]
dpaarsProvisioningArtifactSummaries = lens _dpaarsProvisioningArtifactSummaries (\ s a -> s{_dpaarsProvisioningArtifactSummaries = a}) . _Default . _Coerce;

-- | Tags associated with the product.
dpaarsTags :: Lens' DescribeProductAsAdminResponse [Tag]
dpaarsTags = lens _dpaarsTags (\ s a -> s{_dpaarsTags = a}) . _Default . _Coerce;

-- | -- | The response status code.
dpaarsResponseStatus :: Lens' DescribeProductAsAdminResponse Int
dpaarsResponseStatus = lens _dpaarsResponseStatus (\ s a -> s{_dpaarsResponseStatus = a});

instance NFData DescribeProductAsAdminResponse
