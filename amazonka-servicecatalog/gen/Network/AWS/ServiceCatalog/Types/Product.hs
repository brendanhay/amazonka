{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Product
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.Sum

-- | The access level to limit results.
--
--
--
-- /See:/ 'accessLevelFilter' smart constructor.
data AccessLevelFilter = AccessLevelFilter'
  { _alfValue :: !(Maybe Text)
  , _alfKey   :: !(Maybe AccessLevelFilterKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccessLevelFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alfValue' - Specifies the user to which the access level applies. A value of @Self@ is currently supported.
--
-- * 'alfKey' - Specifies the access level. @Account@ allows results at the account level.  @Role@ allows results based on the federated role of the specified user. @User@ allows results limited to the specified user.
accessLevelFilter
    :: AccessLevelFilter
accessLevelFilter = AccessLevelFilter' {_alfValue = Nothing, _alfKey = Nothing}


-- | Specifies the user to which the access level applies. A value of @Self@ is currently supported.
alfValue :: Lens' AccessLevelFilter (Maybe Text)
alfValue = lens _alfValue (\ s a -> s{_alfValue = a});

-- | Specifies the access level. @Account@ allows results at the account level.  @Role@ allows results based on the federated role of the specified user. @User@ allows results limited to the specified user.
alfKey :: Lens' AccessLevelFilter (Maybe AccessLevelFilterKey)
alfKey = lens _alfKey (\ s a -> s{_alfKey = a});

instance Hashable AccessLevelFilter where

instance NFData AccessLevelFilter where

instance ToJSON AccessLevelFilter where
        toJSON AccessLevelFilter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _alfValue, ("Key" .=) <$> _alfKey])

-- | Detailed constraint information.
--
--
--
-- /See:/ 'constraintDetail' smart constructor.
data ConstraintDetail = ConstraintDetail'
  { _cdConstraintId :: !(Maybe Text)
  , _cdOwner        :: !(Maybe Text)
  , _cdType         :: !(Maybe Text)
  , _cdDescription  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConstraintDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdConstraintId' - The identifier of the constraint.
--
-- * 'cdOwner' - The owner of the constraint.
--
-- * 'cdType' - The type of the constraint.
--
-- * 'cdDescription' - The text description of the constraint.
constraintDetail
    :: ConstraintDetail
constraintDetail =
  ConstraintDetail'
  { _cdConstraintId = Nothing
  , _cdOwner = Nothing
  , _cdType = Nothing
  , _cdDescription = Nothing
  }


-- | The identifier of the constraint.
cdConstraintId :: Lens' ConstraintDetail (Maybe Text)
cdConstraintId = lens _cdConstraintId (\ s a -> s{_cdConstraintId = a});

-- | The owner of the constraint.
cdOwner :: Lens' ConstraintDetail (Maybe Text)
cdOwner = lens _cdOwner (\ s a -> s{_cdOwner = a});

-- | The type of the constraint.
cdType :: Lens' ConstraintDetail (Maybe Text)
cdType = lens _cdType (\ s a -> s{_cdType = a});

-- | The text description of the constraint.
cdDescription :: Lens' ConstraintDetail (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a});

instance FromJSON ConstraintDetail where
        parseJSON
          = withObject "ConstraintDetail"
              (\ x ->
                 ConstraintDetail' <$>
                   (x .:? "ConstraintId") <*> (x .:? "Owner") <*>
                     (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable ConstraintDetail where

instance NFData ConstraintDetail where

-- | An administrator-specified constraint to apply when provisioning a product.
--
--
--
-- /See:/ 'constraintSummary' smart constructor.
data ConstraintSummary = ConstraintSummary'
  { _csType        :: !(Maybe Text)
  , _csDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConstraintSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csType' - The type of the constraint.
--
-- * 'csDescription' - The text description of the constraint.
constraintSummary
    :: ConstraintSummary
constraintSummary =
  ConstraintSummary' {_csType = Nothing, _csDescription = Nothing}


-- | The type of the constraint.
csType :: Lens' ConstraintSummary (Maybe Text)
csType = lens _csType (\ s a -> s{_csType = a});

-- | The text description of the constraint.
csDescription :: Lens' ConstraintSummary (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a});

instance FromJSON ConstraintSummary where
        parseJSON
          = withObject "ConstraintSummary"
              (\ x ->
                 ConstraintSummary' <$>
                   (x .:? "Type") <*> (x .:? "Description"))

instance Hashable ConstraintSummary where

instance NFData ConstraintSummary where

-- | Summary information about a path for a user to have access to a specified product.
--
--
--
-- /See:/ 'launchPathSummary' smart constructor.
data LaunchPathSummary = LaunchPathSummary'
  { _lpsConstraintSummaries :: !(Maybe [ConstraintSummary])
  , _lpsName                :: !(Maybe Text)
  , _lpsId                  :: !(Maybe Text)
  , _lpsTags                :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchPathSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpsConstraintSummaries' - List of constraints on the portfolio-product relationship.
--
-- * 'lpsName' - Corresponds to the name of the portfolio to which the user was assigned.
--
-- * 'lpsId' - The unique identifier of the product path.
--
-- * 'lpsTags' - List of tags used by this launch path.
launchPathSummary
    :: LaunchPathSummary
launchPathSummary =
  LaunchPathSummary'
  { _lpsConstraintSummaries = Nothing
  , _lpsName = Nothing
  , _lpsId = Nothing
  , _lpsTags = Nothing
  }


-- | List of constraints on the portfolio-product relationship.
lpsConstraintSummaries :: Lens' LaunchPathSummary [ConstraintSummary]
lpsConstraintSummaries = lens _lpsConstraintSummaries (\ s a -> s{_lpsConstraintSummaries = a}) . _Default . _Coerce;

-- | Corresponds to the name of the portfolio to which the user was assigned.
lpsName :: Lens' LaunchPathSummary (Maybe Text)
lpsName = lens _lpsName (\ s a -> s{_lpsName = a});

-- | The unique identifier of the product path.
lpsId :: Lens' LaunchPathSummary (Maybe Text)
lpsId = lens _lpsId (\ s a -> s{_lpsId = a});

-- | List of tags used by this launch path.
lpsTags :: Lens' LaunchPathSummary [Tag]
lpsTags = lens _lpsTags (\ s a -> s{_lpsTags = a}) . _Default . _Coerce;

instance FromJSON LaunchPathSummary where
        parseJSON
          = withObject "LaunchPathSummary"
              (\ x ->
                 LaunchPathSummary' <$>
                   (x .:? "ConstraintSummaries" .!= mempty) <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Tags" .!= mempty))

instance Hashable LaunchPathSummary where

instance NFData LaunchPathSummary where

-- | The search filter to limit results when listing request history records.
--
--
--
-- /See:/ 'listRecordHistorySearchFilter' smart constructor.
data ListRecordHistorySearchFilter = ListRecordHistorySearchFilter'
  { _lrhsfValue :: !(Maybe Text)
  , _lrhsfKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRecordHistorySearchFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrhsfValue' - The filter value for @Key@ .
--
-- * 'lrhsfKey' - The filter key.
listRecordHistorySearchFilter
    :: ListRecordHistorySearchFilter
listRecordHistorySearchFilter =
  ListRecordHistorySearchFilter' {_lrhsfValue = Nothing, _lrhsfKey = Nothing}


-- | The filter value for @Key@ .
lrhsfValue :: Lens' ListRecordHistorySearchFilter (Maybe Text)
lrhsfValue = lens _lrhsfValue (\ s a -> s{_lrhsfValue = a});

-- | The filter key.
lrhsfKey :: Lens' ListRecordHistorySearchFilter (Maybe Text)
lrhsfKey = lens _lrhsfKey (\ s a -> s{_lrhsfKey = a});

instance Hashable ListRecordHistorySearchFilter where

instance NFData ListRecordHistorySearchFilter where

instance ToJSON ListRecordHistorySearchFilter where
        toJSON ListRecordHistorySearchFilter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _lrhsfValue,
                  ("Key" .=) <$> _lrhsfKey])

-- | The ListTagOptions filters.
--
--
--
-- /See:/ 'listTagOptionsFilters' smart constructor.
data ListTagOptionsFilters = ListTagOptionsFilters'
  { _ltofValue  :: !(Maybe Text)
  , _ltofActive :: !(Maybe Bool)
  , _ltofKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagOptionsFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltofValue' - The ListTagOptionsFilters value.
--
-- * 'ltofActive' - The ListTagOptionsFilters active state.
--
-- * 'ltofKey' - The ListTagOptionsFilters key.
listTagOptionsFilters
    :: ListTagOptionsFilters
listTagOptionsFilters =
  ListTagOptionsFilters'
  {_ltofValue = Nothing, _ltofActive = Nothing, _ltofKey = Nothing}


-- | The ListTagOptionsFilters value.
ltofValue :: Lens' ListTagOptionsFilters (Maybe Text)
ltofValue = lens _ltofValue (\ s a -> s{_ltofValue = a});

-- | The ListTagOptionsFilters active state.
ltofActive :: Lens' ListTagOptionsFilters (Maybe Bool)
ltofActive = lens _ltofActive (\ s a -> s{_ltofActive = a});

-- | The ListTagOptionsFilters key.
ltofKey :: Lens' ListTagOptionsFilters (Maybe Text)
ltofKey = lens _ltofKey (\ s a -> s{_ltofKey = a});

instance Hashable ListTagOptionsFilters where

instance NFData ListTagOptionsFilters where

instance ToJSON ListTagOptionsFilters where
        toJSON ListTagOptionsFilters'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _ltofValue,
                  ("Active" .=) <$> _ltofActive,
                  ("Key" .=) <$> _ltofKey])

-- | The constraints that the administrator has put on the parameter.
--
--
--
-- /See:/ 'parameterConstraints' smart constructor.
newtype ParameterConstraints = ParameterConstraints'
  { _pcAllowedValues :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcAllowedValues' - The values that the administrator has allowed for the parameter.
parameterConstraints
    :: ParameterConstraints
parameterConstraints = ParameterConstraints' {_pcAllowedValues = Nothing}


-- | The values that the administrator has allowed for the parameter.
pcAllowedValues :: Lens' ParameterConstraints [Text]
pcAllowedValues = lens _pcAllowedValues (\ s a -> s{_pcAllowedValues = a}) . _Default . _Coerce;

instance FromJSON ParameterConstraints where
        parseJSON
          = withObject "ParameterConstraints"
              (\ x ->
                 ParameterConstraints' <$>
                   (x .:? "AllowedValues" .!= mempty))

instance Hashable ParameterConstraints where

instance NFData ParameterConstraints where

-- | Detailed portfolio information.
--
--
--
-- /See:/ 'portfolioDetail' smart constructor.
data PortfolioDetail = PortfolioDetail'
  { _pdARN          :: !(Maybe Text)
  , _pdCreatedTime  :: !(Maybe POSIX)
  , _pdId           :: !(Maybe Text)
  , _pdDisplayName  :: !(Maybe Text)
  , _pdDescription  :: !(Maybe Text)
  , _pdProviderName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PortfolioDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdARN' - The ARN assigned to the portfolio.
--
-- * 'pdCreatedTime' - The UTC timestamp of the creation time.
--
-- * 'pdId' - The identifier for the portfolio.
--
-- * 'pdDisplayName' - The name to use for display purposes.
--
-- * 'pdDescription' - The text description of the portfolio.
--
-- * 'pdProviderName' - The name of the portfolio provider.
portfolioDetail
    :: PortfolioDetail
portfolioDetail =
  PortfolioDetail'
  { _pdARN = Nothing
  , _pdCreatedTime = Nothing
  , _pdId = Nothing
  , _pdDisplayName = Nothing
  , _pdDescription = Nothing
  , _pdProviderName = Nothing
  }


-- | The ARN assigned to the portfolio.
pdARN :: Lens' PortfolioDetail (Maybe Text)
pdARN = lens _pdARN (\ s a -> s{_pdARN = a});

-- | The UTC timestamp of the creation time.
pdCreatedTime :: Lens' PortfolioDetail (Maybe UTCTime)
pdCreatedTime = lens _pdCreatedTime (\ s a -> s{_pdCreatedTime = a}) . mapping _Time;

-- | The identifier for the portfolio.
pdId :: Lens' PortfolioDetail (Maybe Text)
pdId = lens _pdId (\ s a -> s{_pdId = a});

-- | The name to use for display purposes.
pdDisplayName :: Lens' PortfolioDetail (Maybe Text)
pdDisplayName = lens _pdDisplayName (\ s a -> s{_pdDisplayName = a});

-- | The text description of the portfolio.
pdDescription :: Lens' PortfolioDetail (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a});

-- | The name of the portfolio provider.
pdProviderName :: Lens' PortfolioDetail (Maybe Text)
pdProviderName = lens _pdProviderName (\ s a -> s{_pdProviderName = a});

instance FromJSON PortfolioDetail where
        parseJSON
          = withObject "PortfolioDetail"
              (\ x ->
                 PortfolioDetail' <$>
                   (x .:? "ARN") <*> (x .:? "CreatedTime") <*>
                     (x .:? "Id")
                     <*> (x .:? "DisplayName")
                     <*> (x .:? "Description")
                     <*> (x .:? "ProviderName"))

instance Hashable PortfolioDetail where

instance NFData PortfolioDetail where

-- | A principal's ARN and type.
--
--
--
-- /See:/ 'principal' smart constructor.
data Principal = Principal'
  { _pPrincipalType :: !(Maybe PrincipalType)
  , _pPrincipalARN  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Principal' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pPrincipalType' - The principal type. Must be @IAM@
--
-- * 'pPrincipalARN' - The ARN representing the principal (IAM user, role, or group).
principal
    :: Principal
principal = Principal' {_pPrincipalType = Nothing, _pPrincipalARN = Nothing}


-- | The principal type. Must be @IAM@
pPrincipalType :: Lens' Principal (Maybe PrincipalType)
pPrincipalType = lens _pPrincipalType (\ s a -> s{_pPrincipalType = a});

-- | The ARN representing the principal (IAM user, role, or group).
pPrincipalARN :: Lens' Principal (Maybe Text)
pPrincipalARN = lens _pPrincipalARN (\ s a -> s{_pPrincipalARN = a});

instance FromJSON Principal where
        parseJSON
          = withObject "Principal"
              (\ x ->
                 Principal' <$>
                   (x .:? "PrincipalType") <*> (x .:? "PrincipalARN"))

instance Hashable Principal where

instance NFData Principal where

-- | A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.
--
--
--
-- /See:/ 'productViewAggregationValue' smart constructor.
data ProductViewAggregationValue = ProductViewAggregationValue'
  { _pvavValue            :: !(Maybe Text)
  , _pvavApproximateCount :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductViewAggregationValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvavValue' - The value of the product view aggregation.
--
-- * 'pvavApproximateCount' - An approximate count of the products that match the value.
productViewAggregationValue
    :: ProductViewAggregationValue
productViewAggregationValue =
  ProductViewAggregationValue'
  {_pvavValue = Nothing, _pvavApproximateCount = Nothing}


-- | The value of the product view aggregation.
pvavValue :: Lens' ProductViewAggregationValue (Maybe Text)
pvavValue = lens _pvavValue (\ s a -> s{_pvavValue = a});

-- | An approximate count of the products that match the value.
pvavApproximateCount :: Lens' ProductViewAggregationValue (Maybe Int)
pvavApproximateCount = lens _pvavApproximateCount (\ s a -> s{_pvavApproximateCount = a});

instance FromJSON ProductViewAggregationValue where
        parseJSON
          = withObject "ProductViewAggregationValue"
              (\ x ->
                 ProductViewAggregationValue' <$>
                   (x .:? "Value") <*> (x .:? "ApproximateCount"))

instance Hashable ProductViewAggregationValue where

instance NFData ProductViewAggregationValue where

-- | Detailed product view information.
--
--
--
-- /See:/ 'productViewDetail' smart constructor.
data ProductViewDetail = ProductViewDetail'
  { _pvdStatus             :: !(Maybe RequestStatus)
  , _pvdProductViewSummary :: !(Maybe ProductViewSummary)
  , _pvdCreatedTime        :: !(Maybe POSIX)
  , _pvdProductARN         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProductViewDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvdStatus' - Current status of the product. @AVAILABLE@ - Product is available for use. @CREATING@ - Creation of product started, not ready for use. @FAILED@ - Action on product failed.
--
-- * 'pvdProductViewSummary' - The summary metadata about the specified product view.
--
-- * 'pvdCreatedTime' - The UTC timestamp of the creation time.
--
-- * 'pvdProductARN' - The ARN associated with the product.
productViewDetail
    :: ProductViewDetail
productViewDetail =
  ProductViewDetail'
  { _pvdStatus = Nothing
  , _pvdProductViewSummary = Nothing
  , _pvdCreatedTime = Nothing
  , _pvdProductARN = Nothing
  }


-- | Current status of the product. @AVAILABLE@ - Product is available for use. @CREATING@ - Creation of product started, not ready for use. @FAILED@ - Action on product failed.
pvdStatus :: Lens' ProductViewDetail (Maybe RequestStatus)
pvdStatus = lens _pvdStatus (\ s a -> s{_pvdStatus = a});

-- | The summary metadata about the specified product view.
pvdProductViewSummary :: Lens' ProductViewDetail (Maybe ProductViewSummary)
pvdProductViewSummary = lens _pvdProductViewSummary (\ s a -> s{_pvdProductViewSummary = a});

-- | The UTC timestamp of the creation time.
pvdCreatedTime :: Lens' ProductViewDetail (Maybe UTCTime)
pvdCreatedTime = lens _pvdCreatedTime (\ s a -> s{_pvdCreatedTime = a}) . mapping _Time;

-- | The ARN associated with the product.
pvdProductARN :: Lens' ProductViewDetail (Maybe Text)
pvdProductARN = lens _pvdProductARN (\ s a -> s{_pvdProductARN = a});

instance FromJSON ProductViewDetail where
        parseJSON
          = withObject "ProductViewDetail"
              (\ x ->
                 ProductViewDetail' <$>
                   (x .:? "Status") <*> (x .:? "ProductViewSummary") <*>
                     (x .:? "CreatedTime")
                     <*> (x .:? "ProductARN"))

instance Hashable ProductViewDetail where

instance NFData ProductViewDetail where

-- | The summary metadata about the specified product.
--
--
--
-- /See:/ 'productViewSummary' smart constructor.
data ProductViewSummary = ProductViewSummary'
  { _pvsOwner              :: !(Maybe Text)
  , _pvsSupportURL         :: !(Maybe Text)
  , _pvsShortDescription   :: !(Maybe Text)
  , _pvsHasDefaultPath     :: !(Maybe Bool)
  , _pvsDistributor        :: !(Maybe Text)
  , _pvsName               :: !(Maybe Text)
  , _pvsId                 :: !(Maybe Text)
  , _pvsType               :: !(Maybe ProductType)
  , _pvsSupportEmail       :: !(Maybe Text)
  , _pvsProductId          :: !(Maybe Text)
  , _pvsSupportDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
-- * 'pvsHasDefaultPath' - A value of @false@ indicates that the product does not have a default path, while a value of @true@ indicates that it does. If it's false, call 'ListLaunchPaths' to disambiguate between paths. If true, 'ListLaunchPaths' is not required, and the output of the 'ProductViewSummary' operation can be used directly with 'DescribeProvisioningParameters' .
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
productViewSummary
    :: ProductViewSummary
productViewSummary =
  ProductViewSummary'
  { _pvsOwner = Nothing
  , _pvsSupportURL = Nothing
  , _pvsShortDescription = Nothing
  , _pvsHasDefaultPath = Nothing
  , _pvsDistributor = Nothing
  , _pvsName = Nothing
  , _pvsId = Nothing
  , _pvsType = Nothing
  , _pvsSupportEmail = Nothing
  , _pvsProductId = Nothing
  , _pvsSupportDescription = Nothing
  }


-- | The owner of the product. Contact the product administrator for the significance of this value.
pvsOwner :: Lens' ProductViewSummary (Maybe Text)
pvsOwner = lens _pvsOwner (\ s a -> s{_pvsOwner = a});

-- | The URL information to obtain support for this Product.
pvsSupportURL :: Lens' ProductViewSummary (Maybe Text)
pvsSupportURL = lens _pvsSupportURL (\ s a -> s{_pvsSupportURL = a});

-- | Short description of the product.
pvsShortDescription :: Lens' ProductViewSummary (Maybe Text)
pvsShortDescription = lens _pvsShortDescription (\ s a -> s{_pvsShortDescription = a});

-- | A value of @false@ indicates that the product does not have a default path, while a value of @true@ indicates that it does. If it's false, call 'ListLaunchPaths' to disambiguate between paths. If true, 'ListLaunchPaths' is not required, and the output of the 'ProductViewSummary' operation can be used directly with 'DescribeProvisioningParameters' .
pvsHasDefaultPath :: Lens' ProductViewSummary (Maybe Bool)
pvsHasDefaultPath = lens _pvsHasDefaultPath (\ s a -> s{_pvsHasDefaultPath = a});

-- | The distributor of the product. Contact the product administrator for the significance of this value.
pvsDistributor :: Lens' ProductViewSummary (Maybe Text)
pvsDistributor = lens _pvsDistributor (\ s a -> s{_pvsDistributor = a});

-- | The name of the product.
pvsName :: Lens' ProductViewSummary (Maybe Text)
pvsName = lens _pvsName (\ s a -> s{_pvsName = a});

-- | The product view identifier.
pvsId :: Lens' ProductViewSummary (Maybe Text)
pvsId = lens _pvsId (\ s a -> s{_pvsId = a});

-- | The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
pvsType :: Lens' ProductViewSummary (Maybe ProductType)
pvsType = lens _pvsType (\ s a -> s{_pvsType = a});

-- | The email contact information to obtain support for this Product.
pvsSupportEmail :: Lens' ProductViewSummary (Maybe Text)
pvsSupportEmail = lens _pvsSupportEmail (\ s a -> s{_pvsSupportEmail = a});

-- | The product identifier.
pvsProductId :: Lens' ProductViewSummary (Maybe Text)
pvsProductId = lens _pvsProductId (\ s a -> s{_pvsProductId = a});

-- | The description of the support for this Product.
pvsSupportDescription :: Lens' ProductViewSummary (Maybe Text)
pvsSupportDescription = lens _pvsSupportDescription (\ s a -> s{_pvsSupportDescription = a});

instance FromJSON ProductViewSummary where
        parseJSON
          = withObject "ProductViewSummary"
              (\ x ->
                 ProductViewSummary' <$>
                   (x .:? "Owner") <*> (x .:? "SupportUrl") <*>
                     (x .:? "ShortDescription")
                     <*> (x .:? "HasDefaultPath")
                     <*> (x .:? "Distributor")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "SupportEmail")
                     <*> (x .:? "ProductId")
                     <*> (x .:? "SupportDescription"))

instance Hashable ProductViewSummary where

instance NFData ProductViewSummary where

-- | Detailed information about a ProvisionedProduct object.
--
--
--
-- /See:/ 'provisionedProductDetail' smart constructor.
data ProvisionedProductDetail = ProvisionedProductDetail'
  { _ppdIdempotencyToken :: !(Maybe Text)
  , _ppdStatus           :: !(Maybe ProvisionedProductStatus)
  , _ppdARN              :: !(Maybe Text)
  , _ppdCreatedTime      :: !(Maybe POSIX)
  , _ppdStatusMessage    :: !(Maybe Text)
  , _ppdName             :: !(Maybe Text)
  , _ppdLastRecordId     :: !(Maybe Text)
  , _ppdId               :: !(Maybe Text)
  , _ppdType             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedProductDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppdIdempotencyToken' - A token to disambiguate duplicate requests. You can use the same input in multiple requests, provided that you also specify a different idempotency token for each request.
--
-- * 'ppdStatus' - The current status of the ProvisionedProduct. @AVAILABLE@ - Stable state, ready to perform any operation. The most recent action request succeeded and completed. @UNDER_CHANGE@ - Transitive state, operations performed may or may not have valid results. Wait for an @AVAILABLE@ status before performing operations. @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.  @ERROR@ - Something unexpected happened such that the provisioned product exists but the stack is not running. For example, CloudFormation received an invalid parameter value and could not launch the stack.
--
-- * 'ppdARN' - The ARN associated with the ProvisionedProduct object.
--
-- * 'ppdCreatedTime' - The UTC timestamp of the creation time.
--
-- * 'ppdStatusMessage' - The current status message of the ProvisionedProduct.
--
-- * 'ppdName' - The user-friendly name of the ProvisionedProduct object.
--
-- * 'ppdLastRecordId' - The record identifier of the last request performed on this ProvisionedProduct object.
--
-- * 'ppdId' - The identifier of the ProvisionedProduct object.
--
-- * 'ppdType' - The type of the ProvisionedProduct object.
provisionedProductDetail
    :: ProvisionedProductDetail
provisionedProductDetail =
  ProvisionedProductDetail'
  { _ppdIdempotencyToken = Nothing
  , _ppdStatus = Nothing
  , _ppdARN = Nothing
  , _ppdCreatedTime = Nothing
  , _ppdStatusMessage = Nothing
  , _ppdName = Nothing
  , _ppdLastRecordId = Nothing
  , _ppdId = Nothing
  , _ppdType = Nothing
  }


-- | A token to disambiguate duplicate requests. You can use the same input in multiple requests, provided that you also specify a different idempotency token for each request.
ppdIdempotencyToken :: Lens' ProvisionedProductDetail (Maybe Text)
ppdIdempotencyToken = lens _ppdIdempotencyToken (\ s a -> s{_ppdIdempotencyToken = a});

-- | The current status of the ProvisionedProduct. @AVAILABLE@ - Stable state, ready to perform any operation. The most recent action request succeeded and completed. @UNDER_CHANGE@ - Transitive state, operations performed may or may not have valid results. Wait for an @AVAILABLE@ status before performing operations. @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.  @ERROR@ - Something unexpected happened such that the provisioned product exists but the stack is not running. For example, CloudFormation received an invalid parameter value and could not launch the stack.
ppdStatus :: Lens' ProvisionedProductDetail (Maybe ProvisionedProductStatus)
ppdStatus = lens _ppdStatus (\ s a -> s{_ppdStatus = a});

-- | The ARN associated with the ProvisionedProduct object.
ppdARN :: Lens' ProvisionedProductDetail (Maybe Text)
ppdARN = lens _ppdARN (\ s a -> s{_ppdARN = a});

-- | The UTC timestamp of the creation time.
ppdCreatedTime :: Lens' ProvisionedProductDetail (Maybe UTCTime)
ppdCreatedTime = lens _ppdCreatedTime (\ s a -> s{_ppdCreatedTime = a}) . mapping _Time;

-- | The current status message of the ProvisionedProduct.
ppdStatusMessage :: Lens' ProvisionedProductDetail (Maybe Text)
ppdStatusMessage = lens _ppdStatusMessage (\ s a -> s{_ppdStatusMessage = a});

-- | The user-friendly name of the ProvisionedProduct object.
ppdName :: Lens' ProvisionedProductDetail (Maybe Text)
ppdName = lens _ppdName (\ s a -> s{_ppdName = a});

-- | The record identifier of the last request performed on this ProvisionedProduct object.
ppdLastRecordId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdLastRecordId = lens _ppdLastRecordId (\ s a -> s{_ppdLastRecordId = a});

-- | The identifier of the ProvisionedProduct object.
ppdId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdId = lens _ppdId (\ s a -> s{_ppdId = a});

-- | The type of the ProvisionedProduct object.
ppdType :: Lens' ProvisionedProductDetail (Maybe Text)
ppdType = lens _ppdType (\ s a -> s{_ppdType = a});

instance FromJSON ProvisionedProductDetail where
        parseJSON
          = withObject "ProvisionedProductDetail"
              (\ x ->
                 ProvisionedProductDetail' <$>
                   (x .:? "IdempotencyToken") <*> (x .:? "Status") <*>
                     (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "Name")
                     <*> (x .:? "LastRecordId")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type"))

instance Hashable ProvisionedProductDetail where

instance NFData ProvisionedProductDetail where

-- | Contains information indicating the ways in which a product can be provisioned.
--
--
--
-- /See:/ 'provisioningArtifact' smart constructor.
data ProvisioningArtifact = ProvisioningArtifact'
  { _paCreatedTime :: !(Maybe POSIX)
  , _paName        :: !(Maybe Text)
  , _paId          :: !(Maybe Text)
  , _paDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paCreatedTime' - The UTC timestamp of the creation time.
--
-- * 'paName' - The name of the artifact.
--
-- * 'paId' - The identifier for the artifact. This is sometimes referred to as the product version.
--
-- * 'paDescription' - The text description of the artifact.
provisioningArtifact
    :: ProvisioningArtifact
provisioningArtifact =
  ProvisioningArtifact'
  { _paCreatedTime = Nothing
  , _paName = Nothing
  , _paId = Nothing
  , _paDescription = Nothing
  }


-- | The UTC timestamp of the creation time.
paCreatedTime :: Lens' ProvisioningArtifact (Maybe UTCTime)
paCreatedTime = lens _paCreatedTime (\ s a -> s{_paCreatedTime = a}) . mapping _Time;

-- | The name of the artifact.
paName :: Lens' ProvisioningArtifact (Maybe Text)
paName = lens _paName (\ s a -> s{_paName = a});

-- | The identifier for the artifact. This is sometimes referred to as the product version.
paId :: Lens' ProvisioningArtifact (Maybe Text)
paId = lens _paId (\ s a -> s{_paId = a});

-- | The text description of the artifact.
paDescription :: Lens' ProvisioningArtifact (Maybe Text)
paDescription = lens _paDescription (\ s a -> s{_paDescription = a});

instance FromJSON ProvisioningArtifact where
        parseJSON
          = withObject "ProvisioningArtifact"
              (\ x ->
                 ProvisioningArtifact' <$>
                   (x .:? "CreatedTime") <*> (x .:? "Name") <*>
                     (x .:? "Id")
                     <*> (x .:? "Description"))

instance Hashable ProvisioningArtifact where

instance NFData ProvisioningArtifact where

-- | Detailed provisioning artifact information.
--
--
--
-- /See:/ 'provisioningArtifactDetail' smart constructor.
data ProvisioningArtifactDetail = ProvisioningArtifactDetail'
  { _padCreatedTime :: !(Maybe POSIX)
  , _padName        :: !(Maybe Text)
  , _padId          :: !(Maybe Text)
  , _padType        :: !(Maybe ProvisioningArtifactType)
  , _padDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisioningArtifactDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'padCreatedTime' - The UTC timestamp of the creation time.
--
-- * 'padName' - The name assigned to the provisioning artifact.
--
-- * 'padId' - The identifier of the provisioning artifact. This is sometimes referred to as the product version.
--
-- * 'padType' - The type of the provisioning artifact. The following provisioning artifact types are used by AWS Marketplace products: @MARKETPLACE_AMI@ - AMI products. @MARKETPLACE_CAR@ - CAR (Cluster and AWS Resources) products.
--
-- * 'padDescription' - The text description of the provisioning artifact.
provisioningArtifactDetail
    :: ProvisioningArtifactDetail
provisioningArtifactDetail =
  ProvisioningArtifactDetail'
  { _padCreatedTime = Nothing
  , _padName = Nothing
  , _padId = Nothing
  , _padType = Nothing
  , _padDescription = Nothing
  }


-- | The UTC timestamp of the creation time.
padCreatedTime :: Lens' ProvisioningArtifactDetail (Maybe UTCTime)
padCreatedTime = lens _padCreatedTime (\ s a -> s{_padCreatedTime = a}) . mapping _Time;

-- | The name assigned to the provisioning artifact.
padName :: Lens' ProvisioningArtifactDetail (Maybe Text)
padName = lens _padName (\ s a -> s{_padName = a});

-- | The identifier of the provisioning artifact. This is sometimes referred to as the product version.
padId :: Lens' ProvisioningArtifactDetail (Maybe Text)
padId = lens _padId (\ s a -> s{_padId = a});

-- | The type of the provisioning artifact. The following provisioning artifact types are used by AWS Marketplace products: @MARKETPLACE_AMI@ - AMI products. @MARKETPLACE_CAR@ - CAR (Cluster and AWS Resources) products.
padType :: Lens' ProvisioningArtifactDetail (Maybe ProvisioningArtifactType)
padType = lens _padType (\ s a -> s{_padType = a});

-- | The text description of the provisioning artifact.
padDescription :: Lens' ProvisioningArtifactDetail (Maybe Text)
padDescription = lens _padDescription (\ s a -> s{_padDescription = a});

instance FromJSON ProvisioningArtifactDetail where
        parseJSON
          = withObject "ProvisioningArtifactDetail"
              (\ x ->
                 ProvisioningArtifactDetail' <$>
                   (x .:? "CreatedTime") <*> (x .:? "Name") <*>
                     (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable ProvisioningArtifactDetail where

instance NFData ProvisioningArtifactDetail where

-- | A parameter used to successfully provision the product. This value includes a list of allowable values and additional metadata.
--
--
--
-- /See:/ 'provisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { _pIsNoEcho             :: !(Maybe Bool)
  , _pParameterKey         :: !(Maybe Text)
  , _pParameterType        :: !(Maybe Text)
  , _pParameterConstraints :: !(Maybe ParameterConstraints)
  , _pDefaultValue         :: !(Maybe Text)
  , _pDescription          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisioningArtifactParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pIsNoEcho' - If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
--
-- * 'pParameterKey' - The parameter key.
--
-- * 'pParameterType' - The parameter type.
--
-- * 'pParameterConstraints' - The list of constraints that the administrator has put on the parameter.
--
-- * 'pDefaultValue' - The default value for this parameter.
--
-- * 'pDescription' - The text description of the parameter.
provisioningArtifactParameter
    :: ProvisioningArtifactParameter
provisioningArtifactParameter =
  ProvisioningArtifactParameter'
  { _pIsNoEcho = Nothing
  , _pParameterKey = Nothing
  , _pParameterType = Nothing
  , _pParameterConstraints = Nothing
  , _pDefaultValue = Nothing
  , _pDescription = Nothing
  }


-- | If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
pIsNoEcho :: Lens' ProvisioningArtifactParameter (Maybe Bool)
pIsNoEcho = lens _pIsNoEcho (\ s a -> s{_pIsNoEcho = a});

-- | The parameter key.
pParameterKey :: Lens' ProvisioningArtifactParameter (Maybe Text)
pParameterKey = lens _pParameterKey (\ s a -> s{_pParameterKey = a});

-- | The parameter type.
pParameterType :: Lens' ProvisioningArtifactParameter (Maybe Text)
pParameterType = lens _pParameterType (\ s a -> s{_pParameterType = a});

-- | The list of constraints that the administrator has put on the parameter.
pParameterConstraints :: Lens' ProvisioningArtifactParameter (Maybe ParameterConstraints)
pParameterConstraints = lens _pParameterConstraints (\ s a -> s{_pParameterConstraints = a});

-- | The default value for this parameter.
pDefaultValue :: Lens' ProvisioningArtifactParameter (Maybe Text)
pDefaultValue = lens _pDefaultValue (\ s a -> s{_pDefaultValue = a});

-- | The text description of the parameter.
pDescription :: Lens' ProvisioningArtifactParameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a});

instance FromJSON ProvisioningArtifactParameter where
        parseJSON
          = withObject "ProvisioningArtifactParameter"
              (\ x ->
                 ProvisioningArtifactParameter' <$>
                   (x .:? "IsNoEcho") <*> (x .:? "ParameterKey") <*>
                     (x .:? "ParameterType")
                     <*> (x .:? "ParameterConstraints")
                     <*> (x .:? "DefaultValue")
                     <*> (x .:? "Description"))

instance Hashable ProvisioningArtifactParameter where

instance NFData ProvisioningArtifactParameter where

-- | Provisioning artifact properties. For example request JSON, see 'CreateProvisioningArtifact' .
--
--
--
-- /See:/ 'provisioningArtifactProperties' smart constructor.
data ProvisioningArtifactProperties = ProvisioningArtifactProperties'
  { _papName        :: !(Maybe Text)
  , _papType        :: !(Maybe ProvisioningArtifactType)
  , _papDescription :: !(Maybe Text)
  , _papInfo        :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisioningArtifactProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'papName' - The name assigned to the provisioning artifact properties.
--
-- * 'papType' - The type of the provisioning artifact properties. The following provisioning artifact property types are used by AWS Marketplace products: @MARKETPLACE_AMI@ - AMI products. @MARKETPLACE_CAR@ - CAR (Cluster and AWS Resources) products.
--
-- * 'papDescription' - The text description of the provisioning artifact properties.
--
-- * 'papInfo' - Additional information about the provisioning artifact properties. When using this element in a request, you must specify @LoadTemplateFromURL@ . For more information, see 'CreateProvisioningArtifact' .
provisioningArtifactProperties
    :: ProvisioningArtifactProperties
provisioningArtifactProperties =
  ProvisioningArtifactProperties'
  { _papName = Nothing
  , _papType = Nothing
  , _papDescription = Nothing
  , _papInfo = mempty
  }


-- | The name assigned to the provisioning artifact properties.
papName :: Lens' ProvisioningArtifactProperties (Maybe Text)
papName = lens _papName (\ s a -> s{_papName = a});

-- | The type of the provisioning artifact properties. The following provisioning artifact property types are used by AWS Marketplace products: @MARKETPLACE_AMI@ - AMI products. @MARKETPLACE_CAR@ - CAR (Cluster and AWS Resources) products.
papType :: Lens' ProvisioningArtifactProperties (Maybe ProvisioningArtifactType)
papType = lens _papType (\ s a -> s{_papType = a});

-- | The text description of the provisioning artifact properties.
papDescription :: Lens' ProvisioningArtifactProperties (Maybe Text)
papDescription = lens _papDescription (\ s a -> s{_papDescription = a});

-- | Additional information about the provisioning artifact properties. When using this element in a request, you must specify @LoadTemplateFromURL@ . For more information, see 'CreateProvisioningArtifact' .
papInfo :: Lens' ProvisioningArtifactProperties (HashMap Text Text)
papInfo = lens _papInfo (\ s a -> s{_papInfo = a}) . _Map;

instance Hashable ProvisioningArtifactProperties
         where

instance NFData ProvisioningArtifactProperties where

instance ToJSON ProvisioningArtifactProperties where
        toJSON ProvisioningArtifactProperties'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _papName, ("Type" .=) <$> _papType,
                  ("Description" .=) <$> _papDescription,
                  Just ("Info" .= _papInfo)])

-- | Stores summary information about a provisioning artifact.
--
--
--
-- /See:/ 'provisioningArtifactSummary' smart constructor.
data ProvisioningArtifactSummary = ProvisioningArtifactSummary'
  { _pasProvisioningArtifactMetadata :: !(Maybe (Map Text Text))
  , _pasCreatedTime                  :: !(Maybe POSIX)
  , _pasName                         :: !(Maybe Text)
  , _pasId                           :: !(Maybe Text)
  , _pasDescription                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisioningArtifactSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pasProvisioningArtifactMetadata' - The provisioning artifact metadata. This data is used with products created by AWS Marketplace.
--
-- * 'pasCreatedTime' - The UTC timestamp of the creation time.
--
-- * 'pasName' - The name of the provisioning artifact.
--
-- * 'pasId' - The identifier of the provisioning artifact.
--
-- * 'pasDescription' - The description of the provisioning artifact.
provisioningArtifactSummary
    :: ProvisioningArtifactSummary
provisioningArtifactSummary =
  ProvisioningArtifactSummary'
  { _pasProvisioningArtifactMetadata = Nothing
  , _pasCreatedTime = Nothing
  , _pasName = Nothing
  , _pasId = Nothing
  , _pasDescription = Nothing
  }


-- | The provisioning artifact metadata. This data is used with products created by AWS Marketplace.
pasProvisioningArtifactMetadata :: Lens' ProvisioningArtifactSummary (HashMap Text Text)
pasProvisioningArtifactMetadata = lens _pasProvisioningArtifactMetadata (\ s a -> s{_pasProvisioningArtifactMetadata = a}) . _Default . _Map;

-- | The UTC timestamp of the creation time.
pasCreatedTime :: Lens' ProvisioningArtifactSummary (Maybe UTCTime)
pasCreatedTime = lens _pasCreatedTime (\ s a -> s{_pasCreatedTime = a}) . mapping _Time;

-- | The name of the provisioning artifact.
pasName :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasName = lens _pasName (\ s a -> s{_pasName = a});

-- | The identifier of the provisioning artifact.
pasId :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasId = lens _pasId (\ s a -> s{_pasId = a});

-- | The description of the provisioning artifact.
pasDescription :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasDescription = lens _pasDescription (\ s a -> s{_pasDescription = a});

instance FromJSON ProvisioningArtifactSummary where
        parseJSON
          = withObject "ProvisioningArtifactSummary"
              (\ x ->
                 ProvisioningArtifactSummary' <$>
                   (x .:? "ProvisioningArtifactMetadata" .!= mempty) <*>
                     (x .:? "CreatedTime")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Description"))

instance Hashable ProvisioningArtifactSummary where

instance NFData ProvisioningArtifactSummary where

-- | The parameter key-value pairs used to provision a product.
--
--
--
-- /See:/ 'provisioningParameter' smart constructor.
data ProvisioningParameter = ProvisioningParameter'
  { _ppValue :: !(Maybe Text)
  , _ppKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisioningParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppValue' - The value to use for provisioning. Any constraints on this value can be found in @ProvisioningArtifactParameter@ for @Key@ .
--
-- * 'ppKey' - The @ProvisioningArtifactParameter.ParameterKey@ parameter from 'DescribeProvisioningParameters' .
provisioningParameter
    :: ProvisioningParameter
provisioningParameter =
  ProvisioningParameter' {_ppValue = Nothing, _ppKey = Nothing}


-- | The value to use for provisioning. Any constraints on this value can be found in @ProvisioningArtifactParameter@ for @Key@ .
ppValue :: Lens' ProvisioningParameter (Maybe Text)
ppValue = lens _ppValue (\ s a -> s{_ppValue = a});

-- | The @ProvisioningArtifactParameter.ParameterKey@ parameter from 'DescribeProvisioningParameters' .
ppKey :: Lens' ProvisioningParameter (Maybe Text)
ppKey = lens _ppKey (\ s a -> s{_ppKey = a});

instance Hashable ProvisioningParameter where

instance NFData ProvisioningParameter where

instance ToJSON ProvisioningParameter where
        toJSON ProvisioningParameter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _ppValue, ("Key" .=) <$> _ppKey])

-- | The full details of a specific ProvisionedProduct object.
--
--
--
-- /See:/ 'recordDetail' smart constructor.
data RecordDetail = RecordDetail'
  { _rdStatus                 :: !(Maybe RecordStatus)
  , _rdRecordTags             :: !(Maybe [RecordTag])
  , _rdProvisionedProductName :: !(Maybe Text)
  , _rdProvisioningArtifactId :: !(Maybe Text)
  , _rdCreatedTime            :: !(Maybe POSIX)
  , _rdRecordType             :: !(Maybe Text)
  , _rdRecordId               :: !(Maybe Text)
  , _rdProvisionedProductType :: !(Maybe Text)
  , _rdUpdatedTime            :: !(Maybe POSIX)
  , _rdPathId                 :: !(Maybe Text)
  , _rdProvisionedProductId   :: !(Maybe Text)
  , _rdRecordErrors           :: !(Maybe [RecordError])
  , _rdProductId              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdStatus' - The status of the ProvisionedProduct object. @CREATED@ - Request created but the operation has not yet started. @IN_PROGRESS@ - The requested operation is in-progress. @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback. @SUCCEEDED@ - The requested operation has successfully completed. @FAILED@ - The requested operation has completed but has failed. Investigate using the error messages returned.
--
-- * 'rdRecordTags' - List of tags associated with this record.
--
-- * 'rdProvisionedProductName' - The user-friendly name of the ProvisionedProduct object.
--
-- * 'rdProvisioningArtifactId' - The provisioning artifact identifier for this product. This is sometimes referred to as the product version.
--
-- * 'rdCreatedTime' - The UTC timestamp of the creation time.
--
-- * 'rdRecordType' - The record type for this record.
--
-- * 'rdRecordId' - The identifier of the ProvisionedProduct object record.
--
-- * 'rdProvisionedProductType' - The type of the ProvisionedProduct object.
--
-- * 'rdUpdatedTime' - The time when the record for the ProvisionedProduct object was last updated.
--
-- * 'rdPathId' - The identifier of the path for this product's provisioning.
--
-- * 'rdProvisionedProductId' - The identifier of the ProvisionedProduct object.
--
-- * 'rdRecordErrors' - A list of errors that occurred while processing the request.
--
-- * 'rdProductId' - The product identifier.
recordDetail
    :: RecordDetail
recordDetail =
  RecordDetail'
  { _rdStatus = Nothing
  , _rdRecordTags = Nothing
  , _rdProvisionedProductName = Nothing
  , _rdProvisioningArtifactId = Nothing
  , _rdCreatedTime = Nothing
  , _rdRecordType = Nothing
  , _rdRecordId = Nothing
  , _rdProvisionedProductType = Nothing
  , _rdUpdatedTime = Nothing
  , _rdPathId = Nothing
  , _rdProvisionedProductId = Nothing
  , _rdRecordErrors = Nothing
  , _rdProductId = Nothing
  }


-- | The status of the ProvisionedProduct object. @CREATED@ - Request created but the operation has not yet started. @IN_PROGRESS@ - The requested operation is in-progress. @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback. @SUCCEEDED@ - The requested operation has successfully completed. @FAILED@ - The requested operation has completed but has failed. Investigate using the error messages returned.
rdStatus :: Lens' RecordDetail (Maybe RecordStatus)
rdStatus = lens _rdStatus (\ s a -> s{_rdStatus = a});

-- | List of tags associated with this record.
rdRecordTags :: Lens' RecordDetail [RecordTag]
rdRecordTags = lens _rdRecordTags (\ s a -> s{_rdRecordTags = a}) . _Default . _Coerce;

-- | The user-friendly name of the ProvisionedProduct object.
rdProvisionedProductName :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductName = lens _rdProvisionedProductName (\ s a -> s{_rdProvisionedProductName = a});

-- | The provisioning artifact identifier for this product. This is sometimes referred to as the product version.
rdProvisioningArtifactId :: Lens' RecordDetail (Maybe Text)
rdProvisioningArtifactId = lens _rdProvisioningArtifactId (\ s a -> s{_rdProvisioningArtifactId = a});

-- | The UTC timestamp of the creation time.
rdCreatedTime :: Lens' RecordDetail (Maybe UTCTime)
rdCreatedTime = lens _rdCreatedTime (\ s a -> s{_rdCreatedTime = a}) . mapping _Time;

-- | The record type for this record.
rdRecordType :: Lens' RecordDetail (Maybe Text)
rdRecordType = lens _rdRecordType (\ s a -> s{_rdRecordType = a});

-- | The identifier of the ProvisionedProduct object record.
rdRecordId :: Lens' RecordDetail (Maybe Text)
rdRecordId = lens _rdRecordId (\ s a -> s{_rdRecordId = a});

-- | The type of the ProvisionedProduct object.
rdProvisionedProductType :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductType = lens _rdProvisionedProductType (\ s a -> s{_rdProvisionedProductType = a});

-- | The time when the record for the ProvisionedProduct object was last updated.
rdUpdatedTime :: Lens' RecordDetail (Maybe UTCTime)
rdUpdatedTime = lens _rdUpdatedTime (\ s a -> s{_rdUpdatedTime = a}) . mapping _Time;

-- | The identifier of the path for this product's provisioning.
rdPathId :: Lens' RecordDetail (Maybe Text)
rdPathId = lens _rdPathId (\ s a -> s{_rdPathId = a});

-- | The identifier of the ProvisionedProduct object.
rdProvisionedProductId :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductId = lens _rdProvisionedProductId (\ s a -> s{_rdProvisionedProductId = a});

-- | A list of errors that occurred while processing the request.
rdRecordErrors :: Lens' RecordDetail [RecordError]
rdRecordErrors = lens _rdRecordErrors (\ s a -> s{_rdRecordErrors = a}) . _Default . _Coerce;

-- | The product identifier.
rdProductId :: Lens' RecordDetail (Maybe Text)
rdProductId = lens _rdProductId (\ s a -> s{_rdProductId = a});

instance FromJSON RecordDetail where
        parseJSON
          = withObject "RecordDetail"
              (\ x ->
                 RecordDetail' <$>
                   (x .:? "Status") <*> (x .:? "RecordTags" .!= mempty)
                     <*> (x .:? "ProvisionedProductName")
                     <*> (x .:? "ProvisioningArtifactId")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "RecordType")
                     <*> (x .:? "RecordId")
                     <*> (x .:? "ProvisionedProductType")
                     <*> (x .:? "UpdatedTime")
                     <*> (x .:? "PathId")
                     <*> (x .:? "ProvisionedProductId")
                     <*> (x .:? "RecordErrors" .!= mempty)
                     <*> (x .:? "ProductId"))

instance Hashable RecordDetail where

instance NFData RecordDetail where

-- | The error code and description resulting from an operation.
--
--
--
-- /See:/ 'recordError' smart constructor.
data RecordError = RecordError'
  { _reCode        :: !(Maybe Text)
  , _reDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reCode' - The numeric value of the error.
--
-- * 'reDescription' - The text description of the error.
recordError
    :: RecordError
recordError = RecordError' {_reCode = Nothing, _reDescription = Nothing}


-- | The numeric value of the error.
reCode :: Lens' RecordError (Maybe Text)
reCode = lens _reCode (\ s a -> s{_reCode = a});

-- | The text description of the error.
reDescription :: Lens' RecordError (Maybe Text)
reDescription = lens _reDescription (\ s a -> s{_reDescription = a});

instance FromJSON RecordError where
        parseJSON
          = withObject "RecordError"
              (\ x ->
                 RecordError' <$>
                   (x .:? "Code") <*> (x .:? "Description"))

instance Hashable RecordError where

instance NFData RecordError where

-- | An output for the specified Product object created as the result of a request. For example, a CloudFormation-backed product that creates an S3 bucket would have an output for the S3 bucket URL.
--
--
--
-- /See:/ 'recordOutput' smart constructor.
data RecordOutput = RecordOutput'
  { _roOutputValue :: !(Maybe Text)
  , _roOutputKey   :: !(Maybe Text)
  , _roDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roOutputValue' - The output value.
--
-- * 'roOutputKey' - The output key.
--
-- * 'roDescription' - The text description of the output.
recordOutput
    :: RecordOutput
recordOutput =
  RecordOutput'
  {_roOutputValue = Nothing, _roOutputKey = Nothing, _roDescription = Nothing}


-- | The output value.
roOutputValue :: Lens' RecordOutput (Maybe Text)
roOutputValue = lens _roOutputValue (\ s a -> s{_roOutputValue = a});

-- | The output key.
roOutputKey :: Lens' RecordOutput (Maybe Text)
roOutputKey = lens _roOutputKey (\ s a -> s{_roOutputKey = a});

-- | The text description of the output.
roDescription :: Lens' RecordOutput (Maybe Text)
roDescription = lens _roDescription (\ s a -> s{_roDescription = a});

instance FromJSON RecordOutput where
        parseJSON
          = withObject "RecordOutput"
              (\ x ->
                 RecordOutput' <$>
                   (x .:? "OutputValue") <*> (x .:? "OutputKey") <*>
                     (x .:? "Description"))

instance Hashable RecordOutput where

instance NFData RecordOutput where

-- | A tag associated with the record, stored as a key-value pair.
--
--
--
-- /See:/ 'recordTag' smart constructor.
data RecordTag = RecordTag'
  { _rtValue :: !(Maybe Text)
  , _rtKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecordTag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtValue' - The value for this tag.
--
-- * 'rtKey' - The key for this tag.
recordTag
    :: RecordTag
recordTag = RecordTag' {_rtValue = Nothing, _rtKey = Nothing}


-- | The value for this tag.
rtValue :: Lens' RecordTag (Maybe Text)
rtValue = lens _rtValue (\ s a -> s{_rtValue = a});

-- | The key for this tag.
rtKey :: Lens' RecordTag (Maybe Text)
rtKey = lens _rtKey (\ s a -> s{_rtKey = a});

instance FromJSON RecordTag where
        parseJSON
          = withObject "RecordTag"
              (\ x ->
                 RecordTag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable RecordTag where

instance NFData RecordTag where

-- | Detailed resource information.
--
--
--
-- /See:/ 'resourceDetail' smart constructor.
data ResourceDetail = ResourceDetail'
  { _rARN         :: !(Maybe Text)
  , _rCreatedTime :: !(Maybe POSIX)
  , _rName        :: !(Maybe Text)
  , _rId          :: !(Maybe Text)
  , _rDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rARN' - ARN of the resource.
--
-- * 'rCreatedTime' - Creation time of the resource.
--
-- * 'rName' - Name of the resource.
--
-- * 'rId' - Identifier of the resource.
--
-- * 'rDescription' - Description of the resource.
resourceDetail
    :: ResourceDetail
resourceDetail =
  ResourceDetail'
  { _rARN = Nothing
  , _rCreatedTime = Nothing
  , _rName = Nothing
  , _rId = Nothing
  , _rDescription = Nothing
  }


-- | ARN of the resource.
rARN :: Lens' ResourceDetail (Maybe Text)
rARN = lens _rARN (\ s a -> s{_rARN = a});

-- | Creation time of the resource.
rCreatedTime :: Lens' ResourceDetail (Maybe UTCTime)
rCreatedTime = lens _rCreatedTime (\ s a -> s{_rCreatedTime = a}) . mapping _Time;

-- | Name of the resource.
rName :: Lens' ResourceDetail (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a});

-- | Identifier of the resource.
rId :: Lens' ResourceDetail (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a});

-- | Description of the resource.
rDescription :: Lens' ResourceDetail (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a});

instance FromJSON ResourceDetail where
        parseJSON
          = withObject "ResourceDetail"
              (\ x ->
                 ResourceDetail' <$>
                   (x .:? "ARN") <*> (x .:? "CreatedTime") <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Description"))

instance Hashable ResourceDetail where

instance NFData ResourceDetail where

-- | Key-value pairs to associate with this provisioning. These tags are entirely discretionary and are propagated to the resources created in the provisioning.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagKey   :: !Text
  , _tagValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagKey' - The @ProvisioningArtifactParameter.TagKey@ parameter from 'DescribeProvisioningParameters' .
--
-- * 'tagValue' - The desired value for this key.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The @ProvisioningArtifactParameter.TagKey@ parameter from 'DescribeProvisioningParameters' .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

-- | The desired value for this key.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .: "Key") <*> (x .: "Value"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _tagKey),
                  Just ("Value" .= _tagValue)])

-- | The TagOption details.
--
--
--
-- /See:/ 'tagOptionDetail' smart constructor.
data TagOptionDetail = TagOptionDetail'
  { _todValue  :: !(Maybe Text)
  , _todActive :: !(Maybe Bool)
  , _todKey    :: !(Maybe Text)
  , _todId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagOptionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'todValue' - The TagOptionDetail value.
--
-- * 'todActive' - The TagOptionDetail active state.
--
-- * 'todKey' - The TagOptionDetail key.
--
-- * 'todId' - The TagOptionDetail identifier.
tagOptionDetail
    :: TagOptionDetail
tagOptionDetail =
  TagOptionDetail'
  { _todValue = Nothing
  , _todActive = Nothing
  , _todKey = Nothing
  , _todId = Nothing
  }


-- | The TagOptionDetail value.
todValue :: Lens' TagOptionDetail (Maybe Text)
todValue = lens _todValue (\ s a -> s{_todValue = a});

-- | The TagOptionDetail active state.
todActive :: Lens' TagOptionDetail (Maybe Bool)
todActive = lens _todActive (\ s a -> s{_todActive = a});

-- | The TagOptionDetail key.
todKey :: Lens' TagOptionDetail (Maybe Text)
todKey = lens _todKey (\ s a -> s{_todKey = a});

-- | The TagOptionDetail identifier.
todId :: Lens' TagOptionDetail (Maybe Text)
todId = lens _todId (\ s a -> s{_todId = a});

instance FromJSON TagOptionDetail where
        parseJSON
          = withObject "TagOptionDetail"
              (\ x ->
                 TagOptionDetail' <$>
                   (x .:? "Value") <*> (x .:? "Active") <*>
                     (x .:? "Key")
                     <*> (x .:? "Id"))

instance Hashable TagOptionDetail where

instance NFData TagOptionDetail where

-- | The TagOption summary key-value pair.
--
--
--
-- /See:/ 'tagOptionSummary' smart constructor.
data TagOptionSummary = TagOptionSummary'
  { _tosValues :: !(Maybe [Text])
  , _tosKey    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagOptionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tosValues' - The TagOptionSummary value.
--
-- * 'tosKey' - The TagOptionSummary key.
tagOptionSummary
    :: TagOptionSummary
tagOptionSummary = TagOptionSummary' {_tosValues = Nothing, _tosKey = Nothing}


-- | The TagOptionSummary value.
tosValues :: Lens' TagOptionSummary [Text]
tosValues = lens _tosValues (\ s a -> s{_tosValues = a}) . _Default . _Coerce;

-- | The TagOptionSummary key.
tosKey :: Lens' TagOptionSummary (Maybe Text)
tosKey = lens _tosKey (\ s a -> s{_tosKey = a});

instance FromJSON TagOptionSummary where
        parseJSON
          = withObject "TagOptionSummary"
              (\ x ->
                 TagOptionSummary' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable TagOptionSummary where

instance NFData TagOptionSummary where

-- | The parameter key-value pair used to update a ProvisionedProduct object. If @UsePreviousValue@ is set to true, @Value@ is ignored and the value for @Key@ is kept as previously set (current value).
--
--
--
-- /See:/ 'updateProvisioningParameter' smart constructor.
data UpdateProvisioningParameter = UpdateProvisioningParameter'
  { _uppValue            :: !(Maybe Text)
  , _uppKey              :: !(Maybe Text)
  , _uppUsePreviousValue :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProvisioningParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uppValue' - The value to use for updating the product provisioning. Any constraints on this value can be found in the @ProvisioningArtifactParameter@ parameter for @Key@ .
--
-- * 'uppKey' - The @ProvisioningArtifactParameter.ParameterKey@ parameter from 'DescribeProvisioningParameters' .
--
-- * 'uppUsePreviousValue' - If true, uses the currently set value for @Key@ , ignoring @UpdateProvisioningParameter.Value@ .
updateProvisioningParameter
    :: UpdateProvisioningParameter
updateProvisioningParameter =
  UpdateProvisioningParameter'
  {_uppValue = Nothing, _uppKey = Nothing, _uppUsePreviousValue = Nothing}


-- | The value to use for updating the product provisioning. Any constraints on this value can be found in the @ProvisioningArtifactParameter@ parameter for @Key@ .
uppValue :: Lens' UpdateProvisioningParameter (Maybe Text)
uppValue = lens _uppValue (\ s a -> s{_uppValue = a});

-- | The @ProvisioningArtifactParameter.ParameterKey@ parameter from 'DescribeProvisioningParameters' .
uppKey :: Lens' UpdateProvisioningParameter (Maybe Text)
uppKey = lens _uppKey (\ s a -> s{_uppKey = a});

-- | If true, uses the currently set value for @Key@ , ignoring @UpdateProvisioningParameter.Value@ .
uppUsePreviousValue :: Lens' UpdateProvisioningParameter (Maybe Bool)
uppUsePreviousValue = lens _uppUsePreviousValue (\ s a -> s{_uppUsePreviousValue = a});

instance Hashable UpdateProvisioningParameter where

instance NFData UpdateProvisioningParameter where

instance ToJSON UpdateProvisioningParameter where
        toJSON UpdateProvisioningParameter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _uppValue, ("Key" .=) <$> _uppKey,
                  ("UsePreviousValue" .=) <$> _uppUsePreviousValue])

-- | Additional information provided by the administrator.
--
--
--
-- /See:/ 'usageInstruction' smart constructor.
data UsageInstruction = UsageInstruction'
  { _uiValue :: !(Maybe Text)
  , _uiType  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UsageInstruction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiValue' - The usage instruction value for this type.
--
-- * 'uiType' - The usage instruction type for the value.
usageInstruction
    :: UsageInstruction
usageInstruction = UsageInstruction' {_uiValue = Nothing, _uiType = Nothing}


-- | The usage instruction value for this type.
uiValue :: Lens' UsageInstruction (Maybe Text)
uiValue = lens _uiValue (\ s a -> s{_uiValue = a});

-- | The usage instruction type for the value.
uiType :: Lens' UsageInstruction (Maybe Text)
uiType = lens _uiType (\ s a -> s{_uiType = a});

instance FromJSON UsageInstruction where
        parseJSON
          = withObject "UsageInstruction"
              (\ x ->
                 UsageInstruction' <$>
                   (x .:? "Value") <*> (x .:? "Type"))

instance Hashable UsageInstruction where

instance NFData UsageInstruction where
