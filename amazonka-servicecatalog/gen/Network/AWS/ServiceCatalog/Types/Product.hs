{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.Sum

-- | The access level to use to filter results.
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
-- * 'alfValue' - The user to which the access level applies. The only supported value is @Self@ .
--
-- * 'alfKey' - The access level.     * @Account@ - Filter results based on the account.     * @Role@ - Filter results based on the federated role of the specified user.     * @User@ - Filter results based on the specified user.
accessLevelFilter
    :: AccessLevelFilter
accessLevelFilter = AccessLevelFilter' {_alfValue = Nothing, _alfKey = Nothing}


-- | The user to which the access level applies. The only supported value is @Self@ .
alfValue :: Lens' AccessLevelFilter (Maybe Text)
alfValue = lens _alfValue (\ s a -> s{_alfValue = a})

-- | The access level.     * @Account@ - Filter results based on the account.     * @Role@ - Filter results based on the federated role of the specified user.     * @User@ - Filter results based on the specified user.
alfKey :: Lens' AccessLevelFilter (Maybe AccessLevelFilterKey)
alfKey = lens _alfKey (\ s a -> s{_alfKey = a})

instance Hashable AccessLevelFilter where

instance NFData AccessLevelFilter where

instance ToJSON AccessLevelFilter where
        toJSON AccessLevelFilter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _alfValue, ("Key" .=) <$> _alfKey])

-- | Information about a CloudWatch dashboard.
--
--
--
-- /See:/ 'cloudWatchDashboard' smart constructor.
newtype CloudWatchDashboard = CloudWatchDashboard'
  { _cwdName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchDashboard' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwdName' - The name of the CloudWatch dashboard.
cloudWatchDashboard
    :: CloudWatchDashboard
cloudWatchDashboard = CloudWatchDashboard' {_cwdName = Nothing}


-- | The name of the CloudWatch dashboard.
cwdName :: Lens' CloudWatchDashboard (Maybe Text)
cwdName = lens _cwdName (\ s a -> s{_cwdName = a})

instance FromJSON CloudWatchDashboard where
        parseJSON
          = withObject "CloudWatchDashboard"
              (\ x -> CloudWatchDashboard' <$> (x .:? "Name"))

instance Hashable CloudWatchDashboard where

instance NFData CloudWatchDashboard where

-- | Information about a constraint.
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
-- * 'cdType' - The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * @TEMPLATE@
--
-- * 'cdDescription' - The description of the constraint.
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
cdConstraintId = lens _cdConstraintId (\ s a -> s{_cdConstraintId = a})

-- | The owner of the constraint.
cdOwner :: Lens' ConstraintDetail (Maybe Text)
cdOwner = lens _cdOwner (\ s a -> s{_cdOwner = a})

-- | The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * @TEMPLATE@
cdType :: Lens' ConstraintDetail (Maybe Text)
cdType = lens _cdType (\ s a -> s{_cdType = a})

-- | The description of the constraint.
cdDescription :: Lens' ConstraintDetail (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a})

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

-- | Summary information about a constraint.
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
-- * 'csType' - The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * @TEMPLATE@
--
-- * 'csDescription' - The description of the constraint.
constraintSummary
    :: ConstraintSummary
constraintSummary =
  ConstraintSummary' {_csType = Nothing, _csDescription = Nothing}


-- | The type of constraint.     * @LAUNCH@      * @NOTIFICATION@      * @TEMPLATE@
csType :: Lens' ConstraintSummary (Maybe Text)
csType = lens _csType (\ s a -> s{_csType = a})

-- | The description of the constraint.
csDescription :: Lens' ConstraintSummary (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

instance FromJSON ConstraintSummary where
        parseJSON
          = withObject "ConstraintSummary"
              (\ x ->
                 ConstraintSummary' <$>
                   (x .:? "Type") <*> (x .:? "Description"))

instance Hashable ConstraintSummary where

instance NFData ConstraintSummary where

-- | Summary information about a product path for a user.
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
-- * 'lpsConstraintSummaries' - The constraints on the portfolio-product relationship.
--
-- * 'lpsName' - The name of the portfolio to which the user was assigned.
--
-- * 'lpsId' - The identifier of the product path.
--
-- * 'lpsTags' - The tags associated with this product path.
launchPathSummary
    :: LaunchPathSummary
launchPathSummary =
  LaunchPathSummary'
    { _lpsConstraintSummaries = Nothing
    , _lpsName = Nothing
    , _lpsId = Nothing
    , _lpsTags = Nothing
    }


-- | The constraints on the portfolio-product relationship.
lpsConstraintSummaries :: Lens' LaunchPathSummary [ConstraintSummary]
lpsConstraintSummaries = lens _lpsConstraintSummaries (\ s a -> s{_lpsConstraintSummaries = a}) . _Default . _Coerce

-- | The name of the portfolio to which the user was assigned.
lpsName :: Lens' LaunchPathSummary (Maybe Text)
lpsName = lens _lpsName (\ s a -> s{_lpsName = a})

-- | The identifier of the product path.
lpsId :: Lens' LaunchPathSummary (Maybe Text)
lpsId = lens _lpsId (\ s a -> s{_lpsId = a})

-- | The tags associated with this product path.
lpsTags :: Lens' LaunchPathSummary [Tag]
lpsTags = lens _lpsTags (\ s a -> s{_lpsTags = a}) . _Default . _Coerce

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

-- | The search filter to use when listing history records.
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
-- * 'lrhsfValue' - The filter value.
--
-- * 'lrhsfKey' - The filter key.     * @product@ - Filter results based on the specified product identifier.     * @provisionedproduct@ - Filter results based on the provisioned product identifier.
listRecordHistorySearchFilter
    :: ListRecordHistorySearchFilter
listRecordHistorySearchFilter =
  ListRecordHistorySearchFilter' {_lrhsfValue = Nothing, _lrhsfKey = Nothing}


-- | The filter value.
lrhsfValue :: Lens' ListRecordHistorySearchFilter (Maybe Text)
lrhsfValue = lens _lrhsfValue (\ s a -> s{_lrhsfValue = a})

-- | The filter key.     * @product@ - Filter results based on the specified product identifier.     * @provisionedproduct@ - Filter results based on the provisioned product identifier.
lrhsfKey :: Lens' ListRecordHistorySearchFilter (Maybe Text)
lrhsfKey = lens _lrhsfKey (\ s a -> s{_lrhsfKey = a})

instance Hashable ListRecordHistorySearchFilter where

instance NFData ListRecordHistorySearchFilter where

instance ToJSON ListRecordHistorySearchFilter where
        toJSON ListRecordHistorySearchFilter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _lrhsfValue,
                  ("Key" .=) <$> _lrhsfKey])

-- | Filters to use when listing TagOptions.
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
-- * 'ltofValue' - The TagOption value.
--
-- * 'ltofActive' - The active state.
--
-- * 'ltofKey' - The TagOption key.
listTagOptionsFilters
    :: ListTagOptionsFilters
listTagOptionsFilters =
  ListTagOptionsFilters'
    {_ltofValue = Nothing, _ltofActive = Nothing, _ltofKey = Nothing}


-- | The TagOption value.
ltofValue :: Lens' ListTagOptionsFilters (Maybe Text)
ltofValue = lens _ltofValue (\ s a -> s{_ltofValue = a})

-- | The active state.
ltofActive :: Lens' ListTagOptionsFilters (Maybe Bool)
ltofActive = lens _ltofActive (\ s a -> s{_ltofActive = a})

-- | The TagOption key.
ltofKey :: Lens' ListTagOptionsFilters (Maybe Text)
ltofKey = lens _ltofKey (\ s a -> s{_ltofKey = a})

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
pcAllowedValues = lens _pcAllowedValues (\ s a -> s{_pcAllowedValues = a}) . _Default . _Coerce

instance FromJSON ParameterConstraints where
        parseJSON
          = withObject "ParameterConstraints"
              (\ x ->
                 ParameterConstraints' <$>
                   (x .:? "AllowedValues" .!= mempty))

instance Hashable ParameterConstraints where

instance NFData ParameterConstraints where

-- | Information about a portfolio.
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
-- * 'pdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'pdId' - The portfolio identifier.
--
-- * 'pdDisplayName' - The name to use for display purposes.
--
-- * 'pdDescription' - The description of the portfolio.
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
pdARN = lens _pdARN (\ s a -> s{_pdARN = a})

-- | The UTC time stamp of the creation time.
pdCreatedTime :: Lens' PortfolioDetail (Maybe UTCTime)
pdCreatedTime = lens _pdCreatedTime (\ s a -> s{_pdCreatedTime = a}) . mapping _Time

-- | The portfolio identifier.
pdId :: Lens' PortfolioDetail (Maybe Text)
pdId = lens _pdId (\ s a -> s{_pdId = a})

-- | The name to use for display purposes.
pdDisplayName :: Lens' PortfolioDetail (Maybe Text)
pdDisplayName = lens _pdDisplayName (\ s a -> s{_pdDisplayName = a})

-- | The description of the portfolio.
pdDescription :: Lens' PortfolioDetail (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a})

-- | The name of the portfolio provider.
pdProviderName :: Lens' PortfolioDetail (Maybe Text)
pdProviderName = lens _pdProviderName (\ s a -> s{_pdProviderName = a})

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

-- | Information about a principal.
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
-- * 'pPrincipalType' - The principal type. The supported value is @IAM@ .
--
-- * 'pPrincipalARN' - The ARN of the principal (IAM user, role, or group).
principal
    :: Principal
principal = Principal' {_pPrincipalType = Nothing, _pPrincipalARN = Nothing}


-- | The principal type. The supported value is @IAM@ .
pPrincipalType :: Lens' Principal (Maybe PrincipalType)
pPrincipalType = lens _pPrincipalType (\ s a -> s{_pPrincipalType = a})

-- | The ARN of the principal (IAM user, role, or group).
pPrincipalARN :: Lens' Principal (Maybe Text)
pPrincipalARN = lens _pPrincipalARN (\ s a -> s{_pPrincipalARN = a})

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
pvavValue = lens _pvavValue (\ s a -> s{_pvavValue = a})

-- | An approximate count of the products that match the value.
pvavApproximateCount :: Lens' ProductViewAggregationValue (Maybe Int)
pvavApproximateCount = lens _pvavApproximateCount (\ s a -> s{_pvavApproximateCount = a})

instance FromJSON ProductViewAggregationValue where
        parseJSON
          = withObject "ProductViewAggregationValue"
              (\ x ->
                 ProductViewAggregationValue' <$>
                   (x .:? "Value") <*> (x .:? "ApproximateCount"))

instance Hashable ProductViewAggregationValue where

instance NFData ProductViewAggregationValue where

-- | Information about a product view.
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
-- * 'pvdStatus' - The status of the product.     * @AVAILABLE@ - The product is ready for use.     * @CREATING@ - Product creation has started; the product is not ready for use.     * @FAILED@ - An action failed.
--
-- * 'pvdProductViewSummary' - Summary information about the product view.
--
-- * 'pvdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'pvdProductARN' - The ARN of the product.
productViewDetail
    :: ProductViewDetail
productViewDetail =
  ProductViewDetail'
    { _pvdStatus = Nothing
    , _pvdProductViewSummary = Nothing
    , _pvdCreatedTime = Nothing
    , _pvdProductARN = Nothing
    }


-- | The status of the product.     * @AVAILABLE@ - The product is ready for use.     * @CREATING@ - Product creation has started; the product is not ready for use.     * @FAILED@ - An action failed.
pvdStatus :: Lens' ProductViewDetail (Maybe RequestStatus)
pvdStatus = lens _pvdStatus (\ s a -> s{_pvdStatus = a})

-- | Summary information about the product view.
pvdProductViewSummary :: Lens' ProductViewDetail (Maybe ProductViewSummary)
pvdProductViewSummary = lens _pvdProductViewSummary (\ s a -> s{_pvdProductViewSummary = a})

-- | The UTC time stamp of the creation time.
pvdCreatedTime :: Lens' ProductViewDetail (Maybe UTCTime)
pvdCreatedTime = lens _pvdCreatedTime (\ s a -> s{_pvdCreatedTime = a}) . mapping _Time

-- | The ARN of the product.
pvdProductARN :: Lens' ProductViewDetail (Maybe Text)
pvdProductARN = lens _pvdProductARN (\ s a -> s{_pvdProductARN = a})

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

-- | Summary information about a product view.
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
pvsOwner = lens _pvsOwner (\ s a -> s{_pvsOwner = a})

-- | The URL information to obtain support for this Product.
pvsSupportURL :: Lens' ProductViewSummary (Maybe Text)
pvsSupportURL = lens _pvsSupportURL (\ s a -> s{_pvsSupportURL = a})

-- | Short description of the product.
pvsShortDescription :: Lens' ProductViewSummary (Maybe Text)
pvsShortDescription = lens _pvsShortDescription (\ s a -> s{_pvsShortDescription = a})

-- | Indicates whether the product has a default path. If the product does not have a default path, call 'ListLaunchPaths' to disambiguate between paths. Otherwise, 'ListLaunchPaths' is not required, and the output of 'ProductViewSummary' can be used directly with 'DescribeProvisioningParameters' .
pvsHasDefaultPath :: Lens' ProductViewSummary (Maybe Bool)
pvsHasDefaultPath = lens _pvsHasDefaultPath (\ s a -> s{_pvsHasDefaultPath = a})

-- | The distributor of the product. Contact the product administrator for the significance of this value.
pvsDistributor :: Lens' ProductViewSummary (Maybe Text)
pvsDistributor = lens _pvsDistributor (\ s a -> s{_pvsDistributor = a})

-- | The name of the product.
pvsName :: Lens' ProductViewSummary (Maybe Text)
pvsName = lens _pvsName (\ s a -> s{_pvsName = a})

-- | The product view identifier.
pvsId :: Lens' ProductViewSummary (Maybe Text)
pvsId = lens _pvsId (\ s a -> s{_pvsId = a})

-- | The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
pvsType :: Lens' ProductViewSummary (Maybe ProductType)
pvsType = lens _pvsType (\ s a -> s{_pvsType = a})

-- | The email contact information to obtain support for this Product.
pvsSupportEmail :: Lens' ProductViewSummary (Maybe Text)
pvsSupportEmail = lens _pvsSupportEmail (\ s a -> s{_pvsSupportEmail = a})

-- | The product identifier.
pvsProductId :: Lens' ProductViewSummary (Maybe Text)
pvsProductId = lens _pvsProductId (\ s a -> s{_pvsProductId = a})

-- | The description of the support for this Product.
pvsSupportDescription :: Lens' ProductViewSummary (Maybe Text)
pvsSupportDescription = lens _pvsSupportDescription (\ s a -> s{_pvsSupportDescription = a})

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

-- | Information about a provisioned product.
--
--
--
-- /See:/ 'provisionedProductAttribute' smart constructor.
data ProvisionedProductAttribute = ProvisionedProductAttribute'
  { _ppaIdempotencyToken       :: !(Maybe Text)
  , _ppaStatus                 :: !(Maybe ProvisionedProductStatus)
  , _ppaProvisioningArtifactId :: !(Maybe Text)
  , _ppaARN                    :: !(Maybe Text)
  , _ppaCreatedTime            :: !(Maybe POSIX)
  , _ppaUserARN                :: !(Maybe Text)
  , _ppaStatusMessage          :: !(Maybe Text)
  , _ppaName                   :: !(Maybe Text)
  , _ppaLastRecordId           :: !(Maybe Text)
  , _ppaUserARNSession         :: !(Maybe Text)
  , _ppaId                     :: !(Maybe Text)
  , _ppaType                   :: !(Maybe Text)
  , _ppaPhysicalId             :: !(Maybe Text)
  , _ppaProductId              :: !(Maybe Text)
  , _ppaTags                   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedProductAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppaIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- * 'ppaStatus' - The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state, operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred, the provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.
--
-- * 'ppaProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'ppaARN' - The ARN of the provisioned product.
--
-- * 'ppaCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'ppaUserARN' - The Amazon Resource Name (ARN) of the IAM user.
--
-- * 'ppaStatusMessage' - The current status message of the provisioned product.
--
-- * 'ppaName' - The user-friendly name of the provisioned product.
--
-- * 'ppaLastRecordId' - The record identifier of the last request performed on this provisioned product.
--
-- * 'ppaUserARNSession' - The ARN of the IAM user in the session. This ARN might contain a session ID.
--
-- * 'ppaId' - The identifier of the provisioned product.
--
-- * 'ppaType' - The type of provisioned product. The supported value is @CFN_STACK@ .
--
-- * 'ppaPhysicalId' - The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
--
-- * 'ppaProductId' - The product identifier.
--
-- * 'ppaTags' - One or more tags.
provisionedProductAttribute
    :: ProvisionedProductAttribute
provisionedProductAttribute =
  ProvisionedProductAttribute'
    { _ppaIdempotencyToken = Nothing
    , _ppaStatus = Nothing
    , _ppaProvisioningArtifactId = Nothing
    , _ppaARN = Nothing
    , _ppaCreatedTime = Nothing
    , _ppaUserARN = Nothing
    , _ppaStatusMessage = Nothing
    , _ppaName = Nothing
    , _ppaLastRecordId = Nothing
    , _ppaUserARNSession = Nothing
    , _ppaId = Nothing
    , _ppaType = Nothing
    , _ppaPhysicalId = Nothing
    , _ppaProductId = Nothing
    , _ppaTags = Nothing
    }


-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
ppaIdempotencyToken :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaIdempotencyToken = lens _ppaIdempotencyToken (\ s a -> s{_ppaIdempotencyToken = a})

-- | The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state, operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred, the provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.
ppaStatus :: Lens' ProvisionedProductAttribute (Maybe ProvisionedProductStatus)
ppaStatus = lens _ppaStatus (\ s a -> s{_ppaStatus = a})

-- | The identifier of the provisioning artifact.
ppaProvisioningArtifactId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaProvisioningArtifactId = lens _ppaProvisioningArtifactId (\ s a -> s{_ppaProvisioningArtifactId = a})

-- | The ARN of the provisioned product.
ppaARN :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaARN = lens _ppaARN (\ s a -> s{_ppaARN = a})

-- | The UTC time stamp of the creation time.
ppaCreatedTime :: Lens' ProvisionedProductAttribute (Maybe UTCTime)
ppaCreatedTime = lens _ppaCreatedTime (\ s a -> s{_ppaCreatedTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the IAM user.
ppaUserARN :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaUserARN = lens _ppaUserARN (\ s a -> s{_ppaUserARN = a})

-- | The current status message of the provisioned product.
ppaStatusMessage :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaStatusMessage = lens _ppaStatusMessage (\ s a -> s{_ppaStatusMessage = a})

-- | The user-friendly name of the provisioned product.
ppaName :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaName = lens _ppaName (\ s a -> s{_ppaName = a})

-- | The record identifier of the last request performed on this provisioned product.
ppaLastRecordId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaLastRecordId = lens _ppaLastRecordId (\ s a -> s{_ppaLastRecordId = a})

-- | The ARN of the IAM user in the session. This ARN might contain a session ID.
ppaUserARNSession :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaUserARNSession = lens _ppaUserARNSession (\ s a -> s{_ppaUserARNSession = a})

-- | The identifier of the provisioned product.
ppaId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaId = lens _ppaId (\ s a -> s{_ppaId = a})

-- | The type of provisioned product. The supported value is @CFN_STACK@ .
ppaType :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaType = lens _ppaType (\ s a -> s{_ppaType = a})

-- | The assigned identifier for the resource, such as an EC2 instance ID or an S3 bucket name.
ppaPhysicalId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaPhysicalId = lens _ppaPhysicalId (\ s a -> s{_ppaPhysicalId = a})

-- | The product identifier.
ppaProductId :: Lens' ProvisionedProductAttribute (Maybe Text)
ppaProductId = lens _ppaProductId (\ s a -> s{_ppaProductId = a})

-- | One or more tags.
ppaTags :: Lens' ProvisionedProductAttribute [Tag]
ppaTags = lens _ppaTags (\ s a -> s{_ppaTags = a}) . _Default . _Coerce

instance FromJSON ProvisionedProductAttribute where
        parseJSON
          = withObject "ProvisionedProductAttribute"
              (\ x ->
                 ProvisionedProductAttribute' <$>
                   (x .:? "IdempotencyToken") <*> (x .:? "Status") <*>
                     (x .:? "ProvisioningArtifactId")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "UserArn")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "Name")
                     <*> (x .:? "LastRecordId")
                     <*> (x .:? "UserArnSession")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "PhysicalId")
                     <*> (x .:? "ProductId")
                     <*> (x .:? "Tags" .!= mempty))

instance Hashable ProvisionedProductAttribute where

instance NFData ProvisionedProductAttribute where

-- | Information about a provisioned product.
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
-- * 'ppdIdempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- * 'ppdStatus' - The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state, operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred, the provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.
--
-- * 'ppdARN' - The ARN of the provisioned product.
--
-- * 'ppdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'ppdStatusMessage' - The current status message of the provisioned product.
--
-- * 'ppdName' - The user-friendly name of the provisioned product.
--
-- * 'ppdLastRecordId' - The record identifier of the last request performed on this provisioned product.
--
-- * 'ppdId' - The identifier of the provisioned product.
--
-- * 'ppdType' - The type of provisioned product. The supported value is @CFN_STACK@ .
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


-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
ppdIdempotencyToken :: Lens' ProvisionedProductDetail (Maybe Text)
ppdIdempotencyToken = lens _ppdIdempotencyToken (\ s a -> s{_ppdIdempotencyToken = a})

-- | The current status of the provisioned product.     * @AVAILABLE@ - Stable state, ready to perform any operation. The most recent operation succeeded and completed.     * @UNDER_CHANGE@ - Transitive state, operations performed might not have valid results. Wait for an @AVAILABLE@ status before performing operations.     * @TAINTED@ - Stable state, ready to perform any operation. The stack has completed the requested operation but is not exactly what was requested. For example, a request to update to a new version failed and the stack rolled back to the current version.     * @ERROR@ - An unexpected error occurred, the provisioned product exists but the stack is not running. For example, CloudFormation received a parameter value that was not valid and could not launch the stack.
ppdStatus :: Lens' ProvisionedProductDetail (Maybe ProvisionedProductStatus)
ppdStatus = lens _ppdStatus (\ s a -> s{_ppdStatus = a})

-- | The ARN of the provisioned product.
ppdARN :: Lens' ProvisionedProductDetail (Maybe Text)
ppdARN = lens _ppdARN (\ s a -> s{_ppdARN = a})

-- | The UTC time stamp of the creation time.
ppdCreatedTime :: Lens' ProvisionedProductDetail (Maybe UTCTime)
ppdCreatedTime = lens _ppdCreatedTime (\ s a -> s{_ppdCreatedTime = a}) . mapping _Time

-- | The current status message of the provisioned product.
ppdStatusMessage :: Lens' ProvisionedProductDetail (Maybe Text)
ppdStatusMessage = lens _ppdStatusMessage (\ s a -> s{_ppdStatusMessage = a})

-- | The user-friendly name of the provisioned product.
ppdName :: Lens' ProvisionedProductDetail (Maybe Text)
ppdName = lens _ppdName (\ s a -> s{_ppdName = a})

-- | The record identifier of the last request performed on this provisioned product.
ppdLastRecordId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdLastRecordId = lens _ppdLastRecordId (\ s a -> s{_ppdLastRecordId = a})

-- | The identifier of the provisioned product.
ppdId :: Lens' ProvisionedProductDetail (Maybe Text)
ppdId = lens _ppdId (\ s a -> s{_ppdId = a})

-- | The type of provisioned product. The supported value is @CFN_STACK@ .
ppdType :: Lens' ProvisionedProductDetail (Maybe Text)
ppdType = lens _ppdType (\ s a -> s{_ppdType = a})

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

-- | Information about a plan.
--
--
--
-- /See:/ 'provisionedProductPlanDetails' smart constructor.
data ProvisionedProductPlanDetails = ProvisionedProductPlanDetails'
  { _pppdStatus                 :: !(Maybe ProvisionedProductPlanStatus)
  , _pppdProvisionProductId     :: !(Maybe Text)
  , _pppdProvisioningArtifactId :: !(Maybe Text)
  , _pppdProvisionProductName   :: !(Maybe Text)
  , _pppdCreatedTime            :: !(Maybe POSIX)
  , _pppdNotificationARNs       :: !(Maybe [Text])
  , _pppdPlanId                 :: !(Maybe Text)
  , _pppdPlanName               :: !(Maybe Text)
  , _pppdStatusMessage          :: !(Maybe Text)
  , _pppdUpdatedTime            :: !(Maybe POSIX)
  , _pppdPathId                 :: !(Maybe Text)
  , _pppdProvisioningParameters :: !(Maybe [UpdateProvisioningParameter])
  , _pppdPlanType               :: !(Maybe ProvisionedProductPlanType)
  , _pppdProductId              :: !(Maybe Text)
  , _pppdTags                   :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedProductPlanDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pppdStatus' - The status.
--
-- * 'pppdProvisionProductId' - The product identifier.
--
-- * 'pppdProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'pppdProvisionProductName' - The user-friendly name of the provisioned product.
--
-- * 'pppdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'pppdNotificationARNs' - Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
--
-- * 'pppdPlanId' - The plan identifier.
--
-- * 'pppdPlanName' - The name of the plan.
--
-- * 'pppdStatusMessage' - The status message.
--
-- * 'pppdUpdatedTime' - The time when the plan was last updated.
--
-- * 'pppdPathId' - The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
--
-- * 'pppdProvisioningParameters' - Parameters specified by the administrator that are required for provisioning the product.
--
-- * 'pppdPlanType' - The plan type.
--
-- * 'pppdProductId' - The product identifier.
--
-- * 'pppdTags' - One or more tags.
provisionedProductPlanDetails
    :: ProvisionedProductPlanDetails
provisionedProductPlanDetails =
  ProvisionedProductPlanDetails'
    { _pppdStatus = Nothing
    , _pppdProvisionProductId = Nothing
    , _pppdProvisioningArtifactId = Nothing
    , _pppdProvisionProductName = Nothing
    , _pppdCreatedTime = Nothing
    , _pppdNotificationARNs = Nothing
    , _pppdPlanId = Nothing
    , _pppdPlanName = Nothing
    , _pppdStatusMessage = Nothing
    , _pppdUpdatedTime = Nothing
    , _pppdPathId = Nothing
    , _pppdProvisioningParameters = Nothing
    , _pppdPlanType = Nothing
    , _pppdProductId = Nothing
    , _pppdTags = Nothing
    }


-- | The status.
pppdStatus :: Lens' ProvisionedProductPlanDetails (Maybe ProvisionedProductPlanStatus)
pppdStatus = lens _pppdStatus (\ s a -> s{_pppdStatus = a})

-- | The product identifier.
pppdProvisionProductId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProvisionProductId = lens _pppdProvisionProductId (\ s a -> s{_pppdProvisionProductId = a})

-- | The identifier of the provisioning artifact.
pppdProvisioningArtifactId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProvisioningArtifactId = lens _pppdProvisioningArtifactId (\ s a -> s{_pppdProvisioningArtifactId = a})

-- | The user-friendly name of the provisioned product.
pppdProvisionProductName :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProvisionProductName = lens _pppdProvisionProductName (\ s a -> s{_pppdProvisionProductName = a})

-- | The UTC time stamp of the creation time.
pppdCreatedTime :: Lens' ProvisionedProductPlanDetails (Maybe UTCTime)
pppdCreatedTime = lens _pppdCreatedTime (\ s a -> s{_pppdCreatedTime = a}) . mapping _Time

-- | Passed to CloudFormation. The SNS topic ARNs to which to publish stack-related events.
pppdNotificationARNs :: Lens' ProvisionedProductPlanDetails [Text]
pppdNotificationARNs = lens _pppdNotificationARNs (\ s a -> s{_pppdNotificationARNs = a}) . _Default . _Coerce

-- | The plan identifier.
pppdPlanId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdPlanId = lens _pppdPlanId (\ s a -> s{_pppdPlanId = a})

-- | The name of the plan.
pppdPlanName :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdPlanName = lens _pppdPlanName (\ s a -> s{_pppdPlanName = a})

-- | The status message.
pppdStatusMessage :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdStatusMessage = lens _pppdStatusMessage (\ s a -> s{_pppdStatusMessage = a})

-- | The time when the plan was last updated.
pppdUpdatedTime :: Lens' ProvisionedProductPlanDetails (Maybe UTCTime)
pppdUpdatedTime = lens _pppdUpdatedTime (\ s a -> s{_pppdUpdatedTime = a}) . mapping _Time

-- | The path identifier of the product. This value is optional if the product has a default path, and required if the product has more than one path. To list the paths for a product, use 'ListLaunchPaths' .
pppdPathId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdPathId = lens _pppdPathId (\ s a -> s{_pppdPathId = a})

-- | Parameters specified by the administrator that are required for provisioning the product.
pppdProvisioningParameters :: Lens' ProvisionedProductPlanDetails [UpdateProvisioningParameter]
pppdProvisioningParameters = lens _pppdProvisioningParameters (\ s a -> s{_pppdProvisioningParameters = a}) . _Default . _Coerce

-- | The plan type.
pppdPlanType :: Lens' ProvisionedProductPlanDetails (Maybe ProvisionedProductPlanType)
pppdPlanType = lens _pppdPlanType (\ s a -> s{_pppdPlanType = a})

-- | The product identifier.
pppdProductId :: Lens' ProvisionedProductPlanDetails (Maybe Text)
pppdProductId = lens _pppdProductId (\ s a -> s{_pppdProductId = a})

-- | One or more tags.
pppdTags :: Lens' ProvisionedProductPlanDetails [Tag]
pppdTags = lens _pppdTags (\ s a -> s{_pppdTags = a}) . _Default . _Coerce

instance FromJSON ProvisionedProductPlanDetails where
        parseJSON
          = withObject "ProvisionedProductPlanDetails"
              (\ x ->
                 ProvisionedProductPlanDetails' <$>
                   (x .:? "Status") <*> (x .:? "ProvisionProductId") <*>
                     (x .:? "ProvisioningArtifactId")
                     <*> (x .:? "ProvisionProductName")
                     <*> (x .:? "CreatedTime")
                     <*> (x .:? "NotificationArns" .!= mempty)
                     <*> (x .:? "PlanId")
                     <*> (x .:? "PlanName")
                     <*> (x .:? "StatusMessage")
                     <*> (x .:? "UpdatedTime")
                     <*> (x .:? "PathId")
                     <*> (x .:? "ProvisioningParameters" .!= mempty)
                     <*> (x .:? "PlanType")
                     <*> (x .:? "ProductId")
                     <*> (x .:? "Tags" .!= mempty))

instance Hashable ProvisionedProductPlanDetails where

instance NFData ProvisionedProductPlanDetails where

-- | Summary information about a plan.
--
--
--
-- /See:/ 'provisionedProductPlanSummary' smart constructor.
data ProvisionedProductPlanSummary = ProvisionedProductPlanSummary'
  { _pppsProvisionProductId     :: !(Maybe Text)
  , _pppsProvisioningArtifactId :: !(Maybe Text)
  , _pppsProvisionProductName   :: !(Maybe Text)
  , _pppsPlanId                 :: !(Maybe Text)
  , _pppsPlanName               :: !(Maybe Text)
  , _pppsPlanType               :: !(Maybe ProvisionedProductPlanType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisionedProductPlanSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pppsProvisionProductId' - The product identifier.
--
-- * 'pppsProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'pppsProvisionProductName' - The user-friendly name of the provisioned product.
--
-- * 'pppsPlanId' - The plan identifier.
--
-- * 'pppsPlanName' - The name of the plan.
--
-- * 'pppsPlanType' - The plan type.
provisionedProductPlanSummary
    :: ProvisionedProductPlanSummary
provisionedProductPlanSummary =
  ProvisionedProductPlanSummary'
    { _pppsProvisionProductId = Nothing
    , _pppsProvisioningArtifactId = Nothing
    , _pppsProvisionProductName = Nothing
    , _pppsPlanId = Nothing
    , _pppsPlanName = Nothing
    , _pppsPlanType = Nothing
    }


-- | The product identifier.
pppsProvisionProductId :: Lens' ProvisionedProductPlanSummary (Maybe Text)
pppsProvisionProductId = lens _pppsProvisionProductId (\ s a -> s{_pppsProvisionProductId = a})

-- | The identifier of the provisioning artifact.
pppsProvisioningArtifactId :: Lens' ProvisionedProductPlanSummary (Maybe Text)
pppsProvisioningArtifactId = lens _pppsProvisioningArtifactId (\ s a -> s{_pppsProvisioningArtifactId = a})

-- | The user-friendly name of the provisioned product.
pppsProvisionProductName :: Lens' ProvisionedProductPlanSummary (Maybe Text)
pppsProvisionProductName = lens _pppsProvisionProductName (\ s a -> s{_pppsProvisionProductName = a})

-- | The plan identifier.
pppsPlanId :: Lens' ProvisionedProductPlanSummary (Maybe Text)
pppsPlanId = lens _pppsPlanId (\ s a -> s{_pppsPlanId = a})

-- | The name of the plan.
pppsPlanName :: Lens' ProvisionedProductPlanSummary (Maybe Text)
pppsPlanName = lens _pppsPlanName (\ s a -> s{_pppsPlanName = a})

-- | The plan type.
pppsPlanType :: Lens' ProvisionedProductPlanSummary (Maybe ProvisionedProductPlanType)
pppsPlanType = lens _pppsPlanType (\ s a -> s{_pppsPlanType = a})

instance FromJSON ProvisionedProductPlanSummary where
        parseJSON
          = withObject "ProvisionedProductPlanSummary"
              (\ x ->
                 ProvisionedProductPlanSummary' <$>
                   (x .:? "ProvisionProductId") <*>
                     (x .:? "ProvisioningArtifactId")
                     <*> (x .:? "ProvisionProductName")
                     <*> (x .:? "PlanId")
                     <*> (x .:? "PlanName")
                     <*> (x .:? "PlanType"))

instance Hashable ProvisionedProductPlanSummary where

instance NFData ProvisionedProductPlanSummary where

-- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
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
-- * 'paCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'paName' - The name of the provisioning artifact.
--
-- * 'paId' - The identifier of the provisioning artifact.
--
-- * 'paDescription' - The description of the provisioning artifact.
provisioningArtifact
    :: ProvisioningArtifact
provisioningArtifact =
  ProvisioningArtifact'
    { _paCreatedTime = Nothing
    , _paName = Nothing
    , _paId = Nothing
    , _paDescription = Nothing
    }


-- | The UTC time stamp of the creation time.
paCreatedTime :: Lens' ProvisioningArtifact (Maybe UTCTime)
paCreatedTime = lens _paCreatedTime (\ s a -> s{_paCreatedTime = a}) . mapping _Time

-- | The name of the provisioning artifact.
paName :: Lens' ProvisioningArtifact (Maybe Text)
paName = lens _paName (\ s a -> s{_paName = a})

-- | The identifier of the provisioning artifact.
paId :: Lens' ProvisioningArtifact (Maybe Text)
paId = lens _paId (\ s a -> s{_paId = a})

-- | The description of the provisioning artifact.
paDescription :: Lens' ProvisioningArtifact (Maybe Text)
paDescription = lens _paDescription (\ s a -> s{_paDescription = a})

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

-- | Information about a provisioning artifact (also known as a version) for a product.
--
--
--
-- /See:/ 'provisioningArtifactDetail' smart constructor.
data ProvisioningArtifactDetail = ProvisioningArtifactDetail'
  { _padCreatedTime :: !(Maybe POSIX)
  , _padActive      :: !(Maybe Bool)
  , _padName        :: !(Maybe Text)
  , _padId          :: !(Maybe Text)
  , _padType        :: !(Maybe ProvisioningArtifactType)
  , _padDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProvisioningArtifactDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'padCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'padActive' - Indicates whether the product version is active.
--
-- * 'padName' - The name of the provisioning artifact.
--
-- * 'padId' - The identifier of the provisioning artifact.
--
-- * 'padType' - The type of provisioning artifact.     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template     * @MARKETPLACE_AMI@ - AWS Marketplace AMI     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
-- * 'padDescription' - The description of the provisioning artifact.
provisioningArtifactDetail
    :: ProvisioningArtifactDetail
provisioningArtifactDetail =
  ProvisioningArtifactDetail'
    { _padCreatedTime = Nothing
    , _padActive = Nothing
    , _padName = Nothing
    , _padId = Nothing
    , _padType = Nothing
    , _padDescription = Nothing
    }


-- | The UTC time stamp of the creation time.
padCreatedTime :: Lens' ProvisioningArtifactDetail (Maybe UTCTime)
padCreatedTime = lens _padCreatedTime (\ s a -> s{_padCreatedTime = a}) . mapping _Time

-- | Indicates whether the product version is active.
padActive :: Lens' ProvisioningArtifactDetail (Maybe Bool)
padActive = lens _padActive (\ s a -> s{_padActive = a})

-- | The name of the provisioning artifact.
padName :: Lens' ProvisioningArtifactDetail (Maybe Text)
padName = lens _padName (\ s a -> s{_padName = a})

-- | The identifier of the provisioning artifact.
padId :: Lens' ProvisioningArtifactDetail (Maybe Text)
padId = lens _padId (\ s a -> s{_padId = a})

-- | The type of provisioning artifact.     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template     * @MARKETPLACE_AMI@ - AWS Marketplace AMI     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
padType :: Lens' ProvisioningArtifactDetail (Maybe ProvisioningArtifactType)
padType = lens _padType (\ s a -> s{_padType = a})

-- | The description of the provisioning artifact.
padDescription :: Lens' ProvisioningArtifactDetail (Maybe Text)
padDescription = lens _padDescription (\ s a -> s{_padDescription = a})

instance FromJSON ProvisioningArtifactDetail where
        parseJSON
          = withObject "ProvisioningArtifactDetail"
              (\ x ->
                 ProvisioningArtifactDetail' <$>
                   (x .:? "CreatedTime") <*> (x .:? "Active") <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description"))

instance Hashable ProvisioningArtifactDetail where

instance NFData ProvisioningArtifactDetail where

-- | Information about a parameter used to provision a product.
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
-- * 'pParameterConstraints' - Constraints that the administrator has put on a parameter.
--
-- * 'pDefaultValue' - The default value.
--
-- * 'pDescription' - The description of the parameter.
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
pIsNoEcho = lens _pIsNoEcho (\ s a -> s{_pIsNoEcho = a})

-- | The parameter key.
pParameterKey :: Lens' ProvisioningArtifactParameter (Maybe Text)
pParameterKey = lens _pParameterKey (\ s a -> s{_pParameterKey = a})

-- | The parameter type.
pParameterType :: Lens' ProvisioningArtifactParameter (Maybe Text)
pParameterType = lens _pParameterType (\ s a -> s{_pParameterType = a})

-- | Constraints that the administrator has put on a parameter.
pParameterConstraints :: Lens' ProvisioningArtifactParameter (Maybe ParameterConstraints)
pParameterConstraints = lens _pParameterConstraints (\ s a -> s{_pParameterConstraints = a})

-- | The default value.
pDefaultValue :: Lens' ProvisioningArtifactParameter (Maybe Text)
pDefaultValue = lens _pDefaultValue (\ s a -> s{_pDefaultValue = a})

-- | The description of the parameter.
pDescription :: Lens' ProvisioningArtifactParameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

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

-- | Information about a provisioning artifact (also known as a version) for a product.
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
-- * 'papName' - The name of the provisioning artifact (for example, v1 v2beta). No spaces are allowed.
--
-- * 'papType' - The type of provisioning artifact.     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template     * @MARKETPLACE_AMI@ - AWS Marketplace AMI     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
-- * 'papDescription' - The description of the provisioning artifact, including how it differs from the previous provisioning artifact.
--
-- * 'papInfo' - The URL of the CloudFormation template in Amazon S3. Specify the URL in JSON format as follows: @"LoadTemplateFromURL": "https://s3.amazonaws.com/cf-templates-ozkq9d3hgiq2-us-east-1/..."@
provisioningArtifactProperties
    :: ProvisioningArtifactProperties
provisioningArtifactProperties =
  ProvisioningArtifactProperties'
    { _papName = Nothing
    , _papType = Nothing
    , _papDescription = Nothing
    , _papInfo = mempty
    }


-- | The name of the provisioning artifact (for example, v1 v2beta). No spaces are allowed.
papName :: Lens' ProvisioningArtifactProperties (Maybe Text)
papName = lens _papName (\ s a -> s{_papName = a})

-- | The type of provisioning artifact.     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template     * @MARKETPLACE_AMI@ - AWS Marketplace AMI     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
papType :: Lens' ProvisioningArtifactProperties (Maybe ProvisioningArtifactType)
papType = lens _papType (\ s a -> s{_papType = a})

-- | The description of the provisioning artifact, including how it differs from the previous provisioning artifact.
papDescription :: Lens' ProvisioningArtifactProperties (Maybe Text)
papDescription = lens _papDescription (\ s a -> s{_papDescription = a})

-- | The URL of the CloudFormation template in Amazon S3. Specify the URL in JSON format as follows: @"LoadTemplateFromURL": "https://s3.amazonaws.com/cf-templates-ozkq9d3hgiq2-us-east-1/..."@
papInfo :: Lens' ProvisioningArtifactProperties (HashMap Text Text)
papInfo = lens _papInfo (\ s a -> s{_papInfo = a}) . _Map

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

-- | Summary information about a provisioning artifact (also known as a version) for a product.
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
-- * 'pasProvisioningArtifactMetadata' - The metadata for the provisioning artifact. This is used with AWS Marketplace products.
--
-- * 'pasCreatedTime' - The UTC time stamp of the creation time.
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


-- | The metadata for the provisioning artifact. This is used with AWS Marketplace products.
pasProvisioningArtifactMetadata :: Lens' ProvisioningArtifactSummary (HashMap Text Text)
pasProvisioningArtifactMetadata = lens _pasProvisioningArtifactMetadata (\ s a -> s{_pasProvisioningArtifactMetadata = a}) . _Default . _Map

-- | The UTC time stamp of the creation time.
pasCreatedTime :: Lens' ProvisioningArtifactSummary (Maybe UTCTime)
pasCreatedTime = lens _pasCreatedTime (\ s a -> s{_pasCreatedTime = a}) . mapping _Time

-- | The name of the provisioning artifact.
pasName :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasName = lens _pasName (\ s a -> s{_pasName = a})

-- | The identifier of the provisioning artifact.
pasId :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasId = lens _pasId (\ s a -> s{_pasId = a})

-- | The description of the provisioning artifact.
pasDescription :: Lens' ProvisioningArtifactSummary (Maybe Text)
pasDescription = lens _pasDescription (\ s a -> s{_pasDescription = a})

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

-- | Information about a parameter used to provision a product.
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
-- * 'ppValue' - The parameter value.
--
-- * 'ppKey' - The parameter key.
provisioningParameter
    :: ProvisioningParameter
provisioningParameter =
  ProvisioningParameter' {_ppValue = Nothing, _ppKey = Nothing}


-- | The parameter value.
ppValue :: Lens' ProvisioningParameter (Maybe Text)
ppValue = lens _ppValue (\ s a -> s{_ppValue = a})

-- | The parameter key.
ppKey :: Lens' ProvisioningParameter (Maybe Text)
ppKey = lens _ppKey (\ s a -> s{_ppKey = a})

instance Hashable ProvisioningParameter where

instance NFData ProvisioningParameter where

instance ToJSON ProvisioningParameter where
        toJSON ProvisioningParameter'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _ppValue, ("Key" .=) <$> _ppKey])

-- | Information about a request operation.
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
-- * 'rdStatus' - The status of the provisioned product.     * @CREATED@ - The request was created but the operation has not started.     * @IN_PROGRESS@ - The requested operation is in progress.     * @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback.     * @SUCCEEDED@ - The requested operation has successfully completed.     * @FAILED@ - The requested operation has unsuccessfully completed. Investigate using the error messages returned.
--
-- * 'rdRecordTags' - One or more tags.
--
-- * 'rdProvisionedProductName' - The user-friendly name of the provisioned product.
--
-- * 'rdProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'rdCreatedTime' - The UTC time stamp of the creation time.
--
-- * 'rdRecordType' - The record type.     * @PROVISION_PRODUCT@      * @UPDATE_PROVISIONED_PRODUCT@      * @TERMINATE_PROVISIONED_PRODUCT@
--
-- * 'rdRecordId' - The identifier of the record.
--
-- * 'rdProvisionedProductType' - The type of provisioned product. The supported value is @CFN_STACK@ .
--
-- * 'rdUpdatedTime' - The time when the record was last updated.
--
-- * 'rdPathId' - The path identifier.
--
-- * 'rdProvisionedProductId' - The identifier of the provisioned product.
--
-- * 'rdRecordErrors' - The errors that occurred.
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


-- | The status of the provisioned product.     * @CREATED@ - The request was created but the operation has not started.     * @IN_PROGRESS@ - The requested operation is in progress.     * @IN_PROGRESS_IN_ERROR@ - The provisioned product is under change but the requested operation failed and some remediation is occurring. For example, a rollback.     * @SUCCEEDED@ - The requested operation has successfully completed.     * @FAILED@ - The requested operation has unsuccessfully completed. Investigate using the error messages returned.
rdStatus :: Lens' RecordDetail (Maybe RecordStatus)
rdStatus = lens _rdStatus (\ s a -> s{_rdStatus = a})

-- | One or more tags.
rdRecordTags :: Lens' RecordDetail [RecordTag]
rdRecordTags = lens _rdRecordTags (\ s a -> s{_rdRecordTags = a}) . _Default . _Coerce

-- | The user-friendly name of the provisioned product.
rdProvisionedProductName :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductName = lens _rdProvisionedProductName (\ s a -> s{_rdProvisionedProductName = a})

-- | The identifier of the provisioning artifact.
rdProvisioningArtifactId :: Lens' RecordDetail (Maybe Text)
rdProvisioningArtifactId = lens _rdProvisioningArtifactId (\ s a -> s{_rdProvisioningArtifactId = a})

-- | The UTC time stamp of the creation time.
rdCreatedTime :: Lens' RecordDetail (Maybe UTCTime)
rdCreatedTime = lens _rdCreatedTime (\ s a -> s{_rdCreatedTime = a}) . mapping _Time

-- | The record type.     * @PROVISION_PRODUCT@      * @UPDATE_PROVISIONED_PRODUCT@      * @TERMINATE_PROVISIONED_PRODUCT@
rdRecordType :: Lens' RecordDetail (Maybe Text)
rdRecordType = lens _rdRecordType (\ s a -> s{_rdRecordType = a})

-- | The identifier of the record.
rdRecordId :: Lens' RecordDetail (Maybe Text)
rdRecordId = lens _rdRecordId (\ s a -> s{_rdRecordId = a})

-- | The type of provisioned product. The supported value is @CFN_STACK@ .
rdProvisionedProductType :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductType = lens _rdProvisionedProductType (\ s a -> s{_rdProvisionedProductType = a})

-- | The time when the record was last updated.
rdUpdatedTime :: Lens' RecordDetail (Maybe UTCTime)
rdUpdatedTime = lens _rdUpdatedTime (\ s a -> s{_rdUpdatedTime = a}) . mapping _Time

-- | The path identifier.
rdPathId :: Lens' RecordDetail (Maybe Text)
rdPathId = lens _rdPathId (\ s a -> s{_rdPathId = a})

-- | The identifier of the provisioned product.
rdProvisionedProductId :: Lens' RecordDetail (Maybe Text)
rdProvisionedProductId = lens _rdProvisionedProductId (\ s a -> s{_rdProvisionedProductId = a})

-- | The errors that occurred.
rdRecordErrors :: Lens' RecordDetail [RecordError]
rdRecordErrors = lens _rdRecordErrors (\ s a -> s{_rdRecordErrors = a}) . _Default . _Coerce

-- | The product identifier.
rdProductId :: Lens' RecordDetail (Maybe Text)
rdProductId = lens _rdProductId (\ s a -> s{_rdProductId = a})

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
-- * 'reDescription' - The description of the error.
recordError
    :: RecordError
recordError = RecordError' {_reCode = Nothing, _reDescription = Nothing}


-- | The numeric value of the error.
reCode :: Lens' RecordError (Maybe Text)
reCode = lens _reCode (\ s a -> s{_reCode = a})

-- | The description of the error.
reDescription :: Lens' RecordError (Maybe Text)
reDescription = lens _reDescription (\ s a -> s{_reDescription = a})

instance FromJSON RecordError where
        parseJSON
          = withObject "RecordError"
              (\ x ->
                 RecordError' <$>
                   (x .:? "Code") <*> (x .:? "Description"))

instance Hashable RecordError where

instance NFData RecordError where

-- | The output for the product created as the result of a request. For example, the output for a CloudFormation-backed product that creates an S3 bucket would include the S3 bucket URL.
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
-- * 'roDescription' - The description of the output.
recordOutput
    :: RecordOutput
recordOutput =
  RecordOutput'
    {_roOutputValue = Nothing, _roOutputKey = Nothing, _roDescription = Nothing}


-- | The output value.
roOutputValue :: Lens' RecordOutput (Maybe Text)
roOutputValue = lens _roOutputValue (\ s a -> s{_roOutputValue = a})

-- | The output key.
roOutputKey :: Lens' RecordOutput (Maybe Text)
roOutputKey = lens _roOutputKey (\ s a -> s{_roOutputKey = a})

-- | The description of the output.
roDescription :: Lens' RecordOutput (Maybe Text)
roDescription = lens _roDescription (\ s a -> s{_roDescription = a})

instance FromJSON RecordOutput where
        parseJSON
          = withObject "RecordOutput"
              (\ x ->
                 RecordOutput' <$>
                   (x .:? "OutputValue") <*> (x .:? "OutputKey") <*>
                     (x .:? "Description"))

instance Hashable RecordOutput where

instance NFData RecordOutput where

-- | Information about a tag, which is a key-value pair.
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
rtValue = lens _rtValue (\ s a -> s{_rtValue = a})

-- | The key for this tag.
rtKey :: Lens' RecordTag (Maybe Text)
rtKey = lens _rtKey (\ s a -> s{_rtKey = a})

instance FromJSON RecordTag where
        parseJSON
          = withObject "RecordTag"
              (\ x ->
                 RecordTag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable RecordTag where

instance NFData RecordTag where

-- | Information about a resource change that will occur when a plan is executed.
--
--
--
-- /See:/ 'resourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { _rcLogicalResourceId  :: !(Maybe Text)
  , _rcPhysicalResourceId :: !(Maybe Text)
  , _rcResourceType       :: !(Maybe Text)
  , _rcAction             :: !(Maybe ChangeAction)
  , _rcScope              :: !(Maybe [ResourceAttribute])
  , _rcDetails            :: !(Maybe [ResourceChangeDetail])
  , _rcReplacement        :: !(Maybe Replacement)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcLogicalResourceId' - The ID of the resource, as defined in the CloudFormation template.
--
-- * 'rcPhysicalResourceId' - The ID of the resource, if it was already created.
--
-- * 'rcResourceType' - The type of resource.
--
-- * 'rcAction' - The change action.
--
-- * 'rcScope' - The change scope.
--
-- * 'rcDetails' - Information about the resource changes.
--
-- * 'rcReplacement' - If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
resourceChange
    :: ResourceChange
resourceChange =
  ResourceChange'
    { _rcLogicalResourceId = Nothing
    , _rcPhysicalResourceId = Nothing
    , _rcResourceType = Nothing
    , _rcAction = Nothing
    , _rcScope = Nothing
    , _rcDetails = Nothing
    , _rcReplacement = Nothing
    }


-- | The ID of the resource, as defined in the CloudFormation template.
rcLogicalResourceId :: Lens' ResourceChange (Maybe Text)
rcLogicalResourceId = lens _rcLogicalResourceId (\ s a -> s{_rcLogicalResourceId = a})

-- | The ID of the resource, if it was already created.
rcPhysicalResourceId :: Lens' ResourceChange (Maybe Text)
rcPhysicalResourceId = lens _rcPhysicalResourceId (\ s a -> s{_rcPhysicalResourceId = a})

-- | The type of resource.
rcResourceType :: Lens' ResourceChange (Maybe Text)
rcResourceType = lens _rcResourceType (\ s a -> s{_rcResourceType = a})

-- | The change action.
rcAction :: Lens' ResourceChange (Maybe ChangeAction)
rcAction = lens _rcAction (\ s a -> s{_rcAction = a})

-- | The change scope.
rcScope :: Lens' ResourceChange [ResourceAttribute]
rcScope = lens _rcScope (\ s a -> s{_rcScope = a}) . _Default . _Coerce

-- | Information about the resource changes.
rcDetails :: Lens' ResourceChange [ResourceChangeDetail]
rcDetails = lens _rcDetails (\ s a -> s{_rcDetails = a}) . _Default . _Coerce

-- | If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
rcReplacement :: Lens' ResourceChange (Maybe Replacement)
rcReplacement = lens _rcReplacement (\ s a -> s{_rcReplacement = a})

instance FromJSON ResourceChange where
        parseJSON
          = withObject "ResourceChange"
              (\ x ->
                 ResourceChange' <$>
                   (x .:? "LogicalResourceId") <*>
                     (x .:? "PhysicalResourceId")
                     <*> (x .:? "ResourceType")
                     <*> (x .:? "Action")
                     <*> (x .:? "Scope" .!= mempty)
                     <*> (x .:? "Details" .!= mempty)
                     <*> (x .:? "Replacement"))

instance Hashable ResourceChange where

instance NFData ResourceChange where

-- | Information about a change to a resource attribute.
--
--
--
-- /See:/ 'resourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { _rcdCausingEntity :: !(Maybe Text)
  , _rcdEvaluation    :: !(Maybe EvaluationType)
  , _rcdTarget        :: !(Maybe ResourceTargetDefinition)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceChangeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcdCausingEntity' - The ID of the entity that caused the change.
--
-- * 'rcdEvaluation' - For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
--
-- * 'rcdTarget' - Information about the resource attribute to be modified.
resourceChangeDetail
    :: ResourceChangeDetail
resourceChangeDetail =
  ResourceChangeDetail'
    { _rcdCausingEntity = Nothing
    , _rcdEvaluation = Nothing
    , _rcdTarget = Nothing
    }


-- | The ID of the entity that caused the change.
rcdCausingEntity :: Lens' ResourceChangeDetail (Maybe Text)
rcdCausingEntity = lens _rcdCausingEntity (\ s a -> s{_rcdCausingEntity = a})

-- | For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
rcdEvaluation :: Lens' ResourceChangeDetail (Maybe EvaluationType)
rcdEvaluation = lens _rcdEvaluation (\ s a -> s{_rcdEvaluation = a})

-- | Information about the resource attribute to be modified.
rcdTarget :: Lens' ResourceChangeDetail (Maybe ResourceTargetDefinition)
rcdTarget = lens _rcdTarget (\ s a -> s{_rcdTarget = a})

instance FromJSON ResourceChangeDetail where
        parseJSON
          = withObject "ResourceChangeDetail"
              (\ x ->
                 ResourceChangeDetail' <$>
                   (x .:? "CausingEntity") <*> (x .:? "Evaluation") <*>
                     (x .:? "Target"))

instance Hashable ResourceChangeDetail where

instance NFData ResourceChangeDetail where

-- | Information about a resource.
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
-- * 'rARN' - The ARN of the resource.
--
-- * 'rCreatedTime' - The creation time of the resource.
--
-- * 'rName' - The name of the resource.
--
-- * 'rId' - The identifier of the resource.
--
-- * 'rDescription' - The description of the resource.
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


-- | The ARN of the resource.
rARN :: Lens' ResourceDetail (Maybe Text)
rARN = lens _rARN (\ s a -> s{_rARN = a})

-- | The creation time of the resource.
rCreatedTime :: Lens' ResourceDetail (Maybe UTCTime)
rCreatedTime = lens _rCreatedTime (\ s a -> s{_rCreatedTime = a}) . mapping _Time

-- | The name of the resource.
rName :: Lens' ResourceDetail (Maybe Text)
rName = lens _rName (\ s a -> s{_rName = a})

-- | The identifier of the resource.
rId :: Lens' ResourceDetail (Maybe Text)
rId = lens _rId (\ s a -> s{_rId = a})

-- | The description of the resource.
rDescription :: Lens' ResourceDetail (Maybe Text)
rDescription = lens _rDescription (\ s a -> s{_rDescription = a})

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

-- | Information about a change to a resource attribute.
--
--
--
-- /See:/ 'resourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { _rtdAttribute          :: !(Maybe ResourceAttribute)
  , _rtdRequiresRecreation :: !(Maybe RequiresRecreation)
  , _rtdName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTargetDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdAttribute' - The attribute to be changed.
--
-- * 'rtdRequiresRecreation' - If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
--
-- * 'rtdName' - If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
resourceTargetDefinition
    :: ResourceTargetDefinition
resourceTargetDefinition =
  ResourceTargetDefinition'
    { _rtdAttribute = Nothing
    , _rtdRequiresRecreation = Nothing
    , _rtdName = Nothing
    }


-- | The attribute to be changed.
rtdAttribute :: Lens' ResourceTargetDefinition (Maybe ResourceAttribute)
rtdAttribute = lens _rtdAttribute (\ s a -> s{_rtdAttribute = a})

-- | If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
rtdRequiresRecreation :: Lens' ResourceTargetDefinition (Maybe RequiresRecreation)
rtdRequiresRecreation = lens _rtdRequiresRecreation (\ s a -> s{_rtdRequiresRecreation = a})

-- | If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
rtdName :: Lens' ResourceTargetDefinition (Maybe Text)
rtdName = lens _rtdName (\ s a -> s{_rtdName = a})

instance FromJSON ResourceTargetDefinition where
        parseJSON
          = withObject "ResourceTargetDefinition"
              (\ x ->
                 ResourceTargetDefinition' <$>
                   (x .:? "Attribute") <*> (x .:? "RequiresRecreation")
                     <*> (x .:? "Name"))

instance Hashable ResourceTargetDefinition where

instance NFData ResourceTargetDefinition where

-- | Information about a tag. A tag is a key-value pair. Tags are propagated to the resources created when provisioning a product.
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
-- * 'tagKey' - The tag key.
--
-- * 'tagValue' - The value for this key.
tag
    :: Text -- ^ 'tagKey'
    -> Text -- ^ 'tagValue'
    -> Tag
tag pKey_ pValue_ = Tag' {_tagKey = pKey_, _tagValue = pValue_}


-- | The tag key.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

-- | The value for this key.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

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

-- | Information about a TagOption.
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
-- * 'todValue' - The TagOption value.
--
-- * 'todActive' - The TagOption active state.
--
-- * 'todKey' - The TagOption key.
--
-- * 'todId' - The TagOption identifier.
tagOptionDetail
    :: TagOptionDetail
tagOptionDetail =
  TagOptionDetail'
    { _todValue = Nothing
    , _todActive = Nothing
    , _todKey = Nothing
    , _todId = Nothing
    }


-- | The TagOption value.
todValue :: Lens' TagOptionDetail (Maybe Text)
todValue = lens _todValue (\ s a -> s{_todValue = a})

-- | The TagOption active state.
todActive :: Lens' TagOptionDetail (Maybe Bool)
todActive = lens _todActive (\ s a -> s{_todActive = a})

-- | The TagOption key.
todKey :: Lens' TagOptionDetail (Maybe Text)
todKey = lens _todKey (\ s a -> s{_todKey = a})

-- | The TagOption identifier.
todId :: Lens' TagOptionDetail (Maybe Text)
todId = lens _todId (\ s a -> s{_todId = a})

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

-- | Summary information about a TagOption.
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
-- * 'tosValues' - The TagOption value.
--
-- * 'tosKey' - The TagOption key.
tagOptionSummary
    :: TagOptionSummary
tagOptionSummary = TagOptionSummary' {_tosValues = Nothing, _tosKey = Nothing}


-- | The TagOption value.
tosValues :: Lens' TagOptionSummary [Text]
tosValues = lens _tosValues (\ s a -> s{_tosValues = a}) . _Default . _Coerce

-- | The TagOption key.
tosKey :: Lens' TagOptionSummary (Maybe Text)
tosKey = lens _tosKey (\ s a -> s{_tosKey = a})

instance FromJSON TagOptionSummary where
        parseJSON
          = withObject "TagOptionSummary"
              (\ x ->
                 TagOptionSummary' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable TagOptionSummary where

instance NFData TagOptionSummary where

-- | The parameter key-value pair used to update a provisioned product.
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
-- * 'uppValue' - The parameter value.
--
-- * 'uppKey' - The parameter key.
--
-- * 'uppUsePreviousValue' - If set to true, @Value@ is ignored and the previous parameter value is kept.
updateProvisioningParameter
    :: UpdateProvisioningParameter
updateProvisioningParameter =
  UpdateProvisioningParameter'
    {_uppValue = Nothing, _uppKey = Nothing, _uppUsePreviousValue = Nothing}


-- | The parameter value.
uppValue :: Lens' UpdateProvisioningParameter (Maybe Text)
uppValue = lens _uppValue (\ s a -> s{_uppValue = a})

-- | The parameter key.
uppKey :: Lens' UpdateProvisioningParameter (Maybe Text)
uppKey = lens _uppKey (\ s a -> s{_uppKey = a})

-- | If set to true, @Value@ is ignored and the previous parameter value is kept.
uppUsePreviousValue :: Lens' UpdateProvisioningParameter (Maybe Bool)
uppUsePreviousValue = lens _uppUsePreviousValue (\ s a -> s{_uppUsePreviousValue = a})

instance FromJSON UpdateProvisioningParameter where
        parseJSON
          = withObject "UpdateProvisioningParameter"
              (\ x ->
                 UpdateProvisioningParameter' <$>
                   (x .:? "Value") <*> (x .:? "Key") <*>
                     (x .:? "UsePreviousValue"))

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
uiValue = lens _uiValue (\ s a -> s{_uiValue = a})

-- | The usage instruction type for the value.
uiType :: Lens' UsageInstruction (Maybe Text)
uiType = lens _uiType (\ s a -> s{_uiType = a})

instance FromJSON UsageInstruction where
        parseJSON
          = withObject "UsageInstruction"
              (\ x ->
                 UsageInstruction' <$>
                   (x .:? "Value") <*> (x .:? "Type"))

instance Hashable UsageInstruction where

instance NFData UsageInstruction where
