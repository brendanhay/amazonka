{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationPreferences where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user-specified preferences for how AWS CloudFormation performs a stack set operation.
--
--
-- For more information on maximum concurrent accounts and failure tolerance, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> .
--
--
-- /See:/ 'stackSetOperationPreferences' smart constructor.
data StackSetOperationPreferences = StackSetOperationPreferences'
  { _ssopRegionOrder ::
      !(Maybe [Text]),
    _ssopMaxConcurrentCount ::
      !(Maybe Nat),
    _ssopMaxConcurrentPercentage ::
      !(Maybe Nat),
    _ssopFailureToleranceCount ::
      !(Maybe Nat),
    _ssopFailureTolerancePercentage ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackSetOperationPreferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssopRegionOrder' - The order of the Regions in where you want to perform the stack operation.
--
-- * 'ssopMaxConcurrentCount' - The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ . @MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ . Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- * 'ssopMaxConcurrentPercentage' - The maximum percentage of accounts in which to perform this operation at one time. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead. Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
--
-- * 'ssopFailureToleranceCount' - The number of accounts, per Region, for which this operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
--
-- * 'ssopFailureTolerancePercentage' - The percentage of accounts, per Region, for which this stack operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
stackSetOperationPreferences ::
  StackSetOperationPreferences
stackSetOperationPreferences =
  StackSetOperationPreferences'
    { _ssopRegionOrder = Nothing,
      _ssopMaxConcurrentCount = Nothing,
      _ssopMaxConcurrentPercentage = Nothing,
      _ssopFailureToleranceCount = Nothing,
      _ssopFailureTolerancePercentage = Nothing
    }

-- | The order of the Regions in where you want to perform the stack operation.
ssopRegionOrder :: Lens' StackSetOperationPreferences [Text]
ssopRegionOrder = lens _ssopRegionOrder (\s a -> s {_ssopRegionOrder = a}) . _Default . _Coerce

-- | The maximum number of accounts in which to perform this operation at one time. This is dependent on the value of @FailureToleranceCount@ . @MaxConcurrentCount@ is at most one more than the @FailureToleranceCount@ . Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
ssopMaxConcurrentCount :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopMaxConcurrentCount = lens _ssopMaxConcurrentCount (\s a -> s {_ssopMaxConcurrentCount = a}) . mapping _Nat

-- | The maximum percentage of accounts in which to perform this operation at one time. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds down to the next whole number. This is true except in cases where rounding down would result is zero. In this case, CloudFormation sets the number as one instead. Note that this setting lets you specify the /maximum/ for operations. For large deployments, under certain circumstances the actual number of accounts acted upon concurrently may be lower due to service throttling. Conditional: You must specify either @MaxConcurrentCount@ or @MaxConcurrentPercentage@ , but not both.
ssopMaxConcurrentPercentage :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopMaxConcurrentPercentage = lens _ssopMaxConcurrentPercentage (\s a -> s {_ssopMaxConcurrentPercentage = a}) . mapping _Nat

-- | The number of accounts, per Region, for which this operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ (but not both).
ssopFailureToleranceCount :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopFailureToleranceCount = lens _ssopFailureToleranceCount (\s a -> s {_ssopFailureToleranceCount = a}) . mapping _Nat

-- | The percentage of accounts, per Region, for which this stack operation can fail before AWS CloudFormation stops the operation in that Region. If the operation is stopped in a Region, AWS CloudFormation doesn't attempt the operation in any subsequent Regions. When calculating the number of accounts based on the specified percentage, AWS CloudFormation rounds /down/ to the next whole number. Conditional: You must specify either @FailureToleranceCount@ or @FailureTolerancePercentage@ , but not both.
ssopFailureTolerancePercentage :: Lens' StackSetOperationPreferences (Maybe Natural)
ssopFailureTolerancePercentage = lens _ssopFailureTolerancePercentage (\s a -> s {_ssopFailureTolerancePercentage = a}) . mapping _Nat

instance FromXML StackSetOperationPreferences where
  parseXML x =
    StackSetOperationPreferences'
      <$> (x .@? "RegionOrder" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "MaxConcurrentCount")
      <*> (x .@? "MaxConcurrentPercentage")
      <*> (x .@? "FailureToleranceCount")
      <*> (x .@? "FailureTolerancePercentage")

instance Hashable StackSetOperationPreferences

instance NFData StackSetOperationPreferences

instance ToQuery StackSetOperationPreferences where
  toQuery StackSetOperationPreferences' {..} =
    mconcat
      [ "RegionOrder"
          =: toQuery (toQueryList "member" <$> _ssopRegionOrder),
        "MaxConcurrentCount" =: _ssopMaxConcurrentCount,
        "MaxConcurrentPercentage" =: _ssopMaxConcurrentPercentage,
        "FailureToleranceCount" =: _ssopFailureToleranceCount,
        "FailureTolerancePercentage" =: _ssopFailureTolerancePercentage
      ]
