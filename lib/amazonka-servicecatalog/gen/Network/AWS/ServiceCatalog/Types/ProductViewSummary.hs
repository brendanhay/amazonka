{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ProductType

-- | Summary information about a product view.
--
--
--
-- /See:/ 'productViewSummary' smart constructor.
data ProductViewSummary = ProductViewSummary'
  { _pvsOwner ::
      !(Maybe Text),
    _pvsSupportURL :: !(Maybe Text),
    _pvsShortDescription :: !(Maybe Text),
    _pvsHasDefaultPath :: !(Maybe Bool),
    _pvsDistributor :: !(Maybe Text),
    _pvsName :: !(Maybe Text),
    _pvsId :: !(Maybe Text),
    _pvsType :: !(Maybe ProductType),
    _pvsSupportEmail :: !(Maybe Text),
    _pvsProductId :: !(Maybe Text),
    _pvsSupportDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProductViewSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvsOwner' - The owner of the product. Contact the product administrator for the significance of this value.
--
-- * 'pvsSupportURL' - The URL information to obtain support for this Product.
--
-- * 'pvsShortDescription' - Short description of the product.
--
-- * 'pvsHasDefaultPath' - Indicates whether the product has a default path. If the product does not have a default path, call 'ListLaunchPaths' to disambiguate between paths. Otherwise, 'ListLaunchPaths' is not required, and the output of 'ProductViewSummary' can be used directly with 'DescribeProvisioningParameters' .
--
-- * 'pvsDistributor' - The distributor of the product. Contact the product administrator for the significance of this value.
--
-- * 'pvsName' - The name of the product.
--
-- * 'pvsId' - The product view identifier.
--
-- * 'pvsType' - The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
--
-- * 'pvsSupportEmail' - The email contact information to obtain support for this Product.
--
-- * 'pvsProductId' - The product identifier.
--
-- * 'pvsSupportDescription' - The description of the support for this Product.
productViewSummary ::
  ProductViewSummary
productViewSummary =
  ProductViewSummary'
    { _pvsOwner = Nothing,
      _pvsSupportURL = Nothing,
      _pvsShortDescription = Nothing,
      _pvsHasDefaultPath = Nothing,
      _pvsDistributor = Nothing,
      _pvsName = Nothing,
      _pvsId = Nothing,
      _pvsType = Nothing,
      _pvsSupportEmail = Nothing,
      _pvsProductId = Nothing,
      _pvsSupportDescription = Nothing
    }

-- | The owner of the product. Contact the product administrator for the significance of this value.
pvsOwner :: Lens' ProductViewSummary (Maybe Text)
pvsOwner = lens _pvsOwner (\s a -> s {_pvsOwner = a})

-- | The URL information to obtain support for this Product.
pvsSupportURL :: Lens' ProductViewSummary (Maybe Text)
pvsSupportURL = lens _pvsSupportURL (\s a -> s {_pvsSupportURL = a})

-- | Short description of the product.
pvsShortDescription :: Lens' ProductViewSummary (Maybe Text)
pvsShortDescription = lens _pvsShortDescription (\s a -> s {_pvsShortDescription = a})

-- | Indicates whether the product has a default path. If the product does not have a default path, call 'ListLaunchPaths' to disambiguate between paths. Otherwise, 'ListLaunchPaths' is not required, and the output of 'ProductViewSummary' can be used directly with 'DescribeProvisioningParameters' .
pvsHasDefaultPath :: Lens' ProductViewSummary (Maybe Bool)
pvsHasDefaultPath = lens _pvsHasDefaultPath (\s a -> s {_pvsHasDefaultPath = a})

-- | The distributor of the product. Contact the product administrator for the significance of this value.
pvsDistributor :: Lens' ProductViewSummary (Maybe Text)
pvsDistributor = lens _pvsDistributor (\s a -> s {_pvsDistributor = a})

-- | The name of the product.
pvsName :: Lens' ProductViewSummary (Maybe Text)
pvsName = lens _pvsName (\s a -> s {_pvsName = a})

-- | The product view identifier.
pvsId :: Lens' ProductViewSummary (Maybe Text)
pvsId = lens _pvsId (\s a -> s {_pvsId = a})

-- | The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
pvsType :: Lens' ProductViewSummary (Maybe ProductType)
pvsType = lens _pvsType (\s a -> s {_pvsType = a})

-- | The email contact information to obtain support for this Product.
pvsSupportEmail :: Lens' ProductViewSummary (Maybe Text)
pvsSupportEmail = lens _pvsSupportEmail (\s a -> s {_pvsSupportEmail = a})

-- | The product identifier.
pvsProductId :: Lens' ProductViewSummary (Maybe Text)
pvsProductId = lens _pvsProductId (\s a -> s {_pvsProductId = a})

-- | The description of the support for this Product.
pvsSupportDescription :: Lens' ProductViewSummary (Maybe Text)
pvsSupportDescription = lens _pvsSupportDescription (\s a -> s {_pvsSupportDescription = a})

instance FromJSON ProductViewSummary where
  parseJSON =
    withObject
      "ProductViewSummary"
      ( \x ->
          ProductViewSummary'
            <$> (x .:? "Owner")
            <*> (x .:? "SupportUrl")
            <*> (x .:? "ShortDescription")
            <*> (x .:? "HasDefaultPath")
            <*> (x .:? "Distributor")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "Type")
            <*> (x .:? "SupportEmail")
            <*> (x .:? "ProductId")
            <*> (x .:? "SupportDescription")
      )

instance Hashable ProductViewSummary

instance NFData ProductViewSummary
