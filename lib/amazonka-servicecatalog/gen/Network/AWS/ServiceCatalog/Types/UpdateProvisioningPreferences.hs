{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.UpdateProvisioningPreferences where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.StackSetOperationType

-- | The user-defined preferences that will be applied when updating a provisioned product. Not all preferences are applicable to all provisioned product types.
--
--
--
-- /See:/ 'updateProvisioningPreferences' smart constructor.
data UpdateProvisioningPreferences = UpdateProvisioningPreferences'
  { _uppStackSetRegions ::
      !(Maybe [Text]),
    _uppStackSetMaxConcurrencyPercentage ::
      !(Maybe Nat),
    _uppStackSetFailureToleranceCount ::
      !(Maybe Nat),
    _uppStackSetFailureTolerancePercentage ::
      !(Maybe Nat),
    _uppStackSetAccounts ::
      !(Maybe [Text]),
    _uppStackSetMaxConcurrencyCount ::
      !(Maybe Nat),
    _uppStackSetOperationType ::
      !(Maybe StackSetOperationType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProvisioningPreferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uppStackSetRegions' - One or more AWS Regions where the provisioned product will be available. Applicable only to a @CFN_STACKSET@ provisioned product type. The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation. If no values are specified, the default value is all regions from the @STACKSET@ constraint.
--
-- * 'uppStackSetMaxConcurrencyPercentage' - The maximum percentage of accounts in which to perform this operation at one time. When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead. Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- * 'uppStackSetFailureToleranceCount' - The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both. The default value is @0@ if no value is specified.
--
-- * 'uppStackSetFailureTolerancePercentage' - The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions. When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
--
-- * 'uppStackSetAccounts' - One or more AWS accounts that will have access to the provisioned product. Applicable only to a @CFN_STACKSET@ provisioned product type. The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation. If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
--
-- * 'uppStackSetMaxConcurrencyCount' - The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ . Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
--
-- * 'uppStackSetOperationType' - Determines what action AWS Service Catalog performs to a stack set or a stack instance represented by the provisioned product. The default value is @UPDATE@ if nothing is specified. Applicable only to a @CFN_STACKSET@ provisioned product type.     * CREATE    * Creates a new stack instance in the stack set represented by the provisioned product. In this case, only new stack instances are created based on accounts and regions; if new ProductId or ProvisioningArtifactID are passed, they will be ignored.     * UPDATE    * Updates the stack set represented by the provisioned product and also its stack instances.     * DELETE    * Deletes a stack instance in the stack set represented by the provisioned product.
updateProvisioningPreferences ::
  UpdateProvisioningPreferences
updateProvisioningPreferences =
  UpdateProvisioningPreferences'
    { _uppStackSetRegions = Nothing,
      _uppStackSetMaxConcurrencyPercentage = Nothing,
      _uppStackSetFailureToleranceCount = Nothing,
      _uppStackSetFailureTolerancePercentage = Nothing,
      _uppStackSetAccounts = Nothing,
      _uppStackSetMaxConcurrencyCount = Nothing,
      _uppStackSetOperationType = Nothing
    }

-- | One or more AWS Regions where the provisioned product will be available. Applicable only to a @CFN_STACKSET@ provisioned product type. The specified regions should be within the list of regions from the @STACKSET@ constraint. To get the list of regions in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation. If no values are specified, the default value is all regions from the @STACKSET@ constraint.
uppStackSetRegions :: Lens' UpdateProvisioningPreferences [Text]
uppStackSetRegions = lens _uppStackSetRegions (\s a -> s {_uppStackSetRegions = a}) . _Default . _Coerce

-- | The maximum percentage of accounts in which to perform this operation at one time. When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, AWS Service Catalog sets the number as @1@ instead. Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
uppStackSetMaxConcurrencyPercentage :: Lens' UpdateProvisioningPreferences (Maybe Natural)
uppStackSetMaxConcurrencyPercentage = lens _uppStackSetMaxConcurrencyPercentage (\s a -> s {_uppStackSetMaxConcurrencyPercentage = a}) . mapping _Nat

-- | The number of accounts, per region, for which this operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both. The default value is @0@ if no value is specified.
uppStackSetFailureToleranceCount :: Lens' UpdateProvisioningPreferences (Maybe Natural)
uppStackSetFailureToleranceCount = lens _uppStackSetFailureToleranceCount (\s a -> s {_uppStackSetFailureToleranceCount = a}) . mapping _Nat

-- | The percentage of accounts, per region, for which this stack operation can fail before AWS Service Catalog stops the operation in that region. If the operation is stopped in a region, AWS Service Catalog doesn't attempt the operation in any subsequent regions. When calculating the number of accounts based on the specified percentage, AWS Service Catalog rounds down to the next whole number. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetFailureToleranceCount@ or @StackSetFailureTolerancePercentage@ , but not both.
uppStackSetFailureTolerancePercentage :: Lens' UpdateProvisioningPreferences (Maybe Natural)
uppStackSetFailureTolerancePercentage = lens _uppStackSetFailureTolerancePercentage (\s a -> s {_uppStackSetFailureTolerancePercentage = a}) . mapping _Nat

-- | One or more AWS accounts that will have access to the provisioned product. Applicable only to a @CFN_STACKSET@ provisioned product type. The AWS accounts specified should be within the list of accounts in the @STACKSET@ constraint. To get the list of accounts in the @STACKSET@ constraint, use the @DescribeProvisioningParameters@ operation. If no values are specified, the default value is all accounts from the @STACKSET@ constraint.
uppStackSetAccounts :: Lens' UpdateProvisioningPreferences [Text]
uppStackSetAccounts = lens _uppStackSetAccounts (\s a -> s {_uppStackSetAccounts = a}) . _Default . _Coerce

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @StackSetFailureToleranceCount@ . @StackSetMaxConcurrentCount@ is at most one more than the @StackSetFailureToleranceCount@ . Note that this setting lets you specify the maximum for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Applicable only to a @CFN_STACKSET@ provisioned product type. Conditional: You must specify either @StackSetMaxConcurrentCount@ or @StackSetMaxConcurrentPercentage@ , but not both.
uppStackSetMaxConcurrencyCount :: Lens' UpdateProvisioningPreferences (Maybe Natural)
uppStackSetMaxConcurrencyCount = lens _uppStackSetMaxConcurrencyCount (\s a -> s {_uppStackSetMaxConcurrencyCount = a}) . mapping _Nat

-- | Determines what action AWS Service Catalog performs to a stack set or a stack instance represented by the provisioned product. The default value is @UPDATE@ if nothing is specified. Applicable only to a @CFN_STACKSET@ provisioned product type.     * CREATE    * Creates a new stack instance in the stack set represented by the provisioned product. In this case, only new stack instances are created based on accounts and regions; if new ProductId or ProvisioningArtifactID are passed, they will be ignored.     * UPDATE    * Updates the stack set represented by the provisioned product and also its stack instances.     * DELETE    * Deletes a stack instance in the stack set represented by the provisioned product.
uppStackSetOperationType :: Lens' UpdateProvisioningPreferences (Maybe StackSetOperationType)
uppStackSetOperationType = lens _uppStackSetOperationType (\s a -> s {_uppStackSetOperationType = a})

instance Hashable UpdateProvisioningPreferences

instance NFData UpdateProvisioningPreferences

instance ToJSON UpdateProvisioningPreferences where
  toJSON UpdateProvisioningPreferences' {..} =
    object
      ( catMaybes
          [ ("StackSetRegions" .=) <$> _uppStackSetRegions,
            ("StackSetMaxConcurrencyPercentage" .=)
              <$> _uppStackSetMaxConcurrencyPercentage,
            ("StackSetFailureToleranceCount" .=)
              <$> _uppStackSetFailureToleranceCount,
            ("StackSetFailureTolerancePercentage" .=)
              <$> _uppStackSetFailureTolerancePercentage,
            ("StackSetAccounts" .=) <$> _uppStackSetAccounts,
            ("StackSetMaxConcurrencyCount" .=)
              <$> _uppStackSetMaxConcurrencyCount,
            ("StackSetOperationType" .=) <$> _uppStackSetOperationType
          ]
      )
