{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProduct
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Network.AWS.ServiceCatalog.DescribeProduct
  ( -- * Creating a Request
    describeProduct,
    DescribeProduct,

    -- * Request Lenses
    dpName,
    dpAcceptLanguage,
    dpId,

    -- * Destructuring the Response
    describeProductResponse,
    DescribeProductResponse,

    -- * Response Lenses
    ddrsProductViewSummary,
    ddrsProvisioningArtifacts,
    ddrsLaunchPaths,
    ddrsBudgets,
    ddrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'describeProduct' smart constructor.
data DescribeProduct = DescribeProduct'
  { _dpName :: !(Maybe Text),
    _dpAcceptLanguage :: !(Maybe Text),
    _dpId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpName' - The product name.
--
-- * 'dpAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpId' - The product identifier.
describeProduct ::
  DescribeProduct
describeProduct =
  DescribeProduct'
    { _dpName = Nothing,
      _dpAcceptLanguage = Nothing,
      _dpId = Nothing
    }

-- | The product name.
dpName :: Lens' DescribeProduct (Maybe Text)
dpName = lens _dpName (\s a -> s {_dpName = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpAcceptLanguage :: Lens' DescribeProduct (Maybe Text)
dpAcceptLanguage = lens _dpAcceptLanguage (\s a -> s {_dpAcceptLanguage = a})

-- | The product identifier.
dpId :: Lens' DescribeProduct (Maybe Text)
dpId = lens _dpId (\s a -> s {_dpId = a})

instance AWSRequest DescribeProduct where
  type Rs DescribeProduct = DescribeProductResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          DescribeProductResponse'
            <$> (x .?> "ProductViewSummary")
            <*> (x .?> "ProvisioningArtifacts" .!@ mempty)
            <*> (x .?> "LaunchPaths" .!@ mempty)
            <*> (x .?> "Budgets" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeProduct

instance NFData DescribeProduct

instance ToHeaders DescribeProduct where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWS242ServiceCatalogService.DescribeProduct" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeProduct where
  toJSON DescribeProduct' {..} =
    object
      ( catMaybes
          [ ("Name" .=) <$> _dpName,
            ("AcceptLanguage" .=) <$> _dpAcceptLanguage,
            ("Id" .=) <$> _dpId
          ]
      )

instance ToPath DescribeProduct where
  toPath = const "/"

instance ToQuery DescribeProduct where
  toQuery = const mempty

-- | /See:/ 'describeProductResponse' smart constructor.
data DescribeProductResponse = DescribeProductResponse'
  { _ddrsProductViewSummary ::
      !(Maybe ProductViewSummary),
    _ddrsProvisioningArtifacts ::
      !(Maybe [ProvisioningArtifact]),
    _ddrsLaunchPaths :: !(Maybe [LaunchPath]),
    _ddrsBudgets :: !(Maybe [BudgetDetail]),
    _ddrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrsProductViewSummary' - Summary information about the product view.
--
-- * 'ddrsProvisioningArtifacts' - Information about the provisioning artifacts for the specified product.
--
-- * 'ddrsLaunchPaths' - Information about the associated launch paths.
--
-- * 'ddrsBudgets' - Information about the associated budgets.
--
-- * 'ddrsResponseStatus' - -- | The response status code.
describeProductResponse ::
  -- | 'ddrsResponseStatus'
  Int ->
  DescribeProductResponse
describeProductResponse pResponseStatus_ =
  DescribeProductResponse'
    { _ddrsProductViewSummary = Nothing,
      _ddrsProvisioningArtifacts = Nothing,
      _ddrsLaunchPaths = Nothing,
      _ddrsBudgets = Nothing,
      _ddrsResponseStatus = pResponseStatus_
    }

-- | Summary information about the product view.
ddrsProductViewSummary :: Lens' DescribeProductResponse (Maybe ProductViewSummary)
ddrsProductViewSummary = lens _ddrsProductViewSummary (\s a -> s {_ddrsProductViewSummary = a})

-- | Information about the provisioning artifacts for the specified product.
ddrsProvisioningArtifacts :: Lens' DescribeProductResponse [ProvisioningArtifact]
ddrsProvisioningArtifacts = lens _ddrsProvisioningArtifacts (\s a -> s {_ddrsProvisioningArtifacts = a}) . _Default . _Coerce

-- | Information about the associated launch paths.
ddrsLaunchPaths :: Lens' DescribeProductResponse [LaunchPath]
ddrsLaunchPaths = lens _ddrsLaunchPaths (\s a -> s {_ddrsLaunchPaths = a}) . _Default . _Coerce

-- | Information about the associated budgets.
ddrsBudgets :: Lens' DescribeProductResponse [BudgetDetail]
ddrsBudgets = lens _ddrsBudgets (\s a -> s {_ddrsBudgets = a}) . _Default . _Coerce

-- | -- | The response status code.
ddrsResponseStatus :: Lens' DescribeProductResponse Int
ddrsResponseStatus = lens _ddrsResponseStatus (\s a -> s {_ddrsResponseStatus = a})

instance NFData DescribeProductResponse
