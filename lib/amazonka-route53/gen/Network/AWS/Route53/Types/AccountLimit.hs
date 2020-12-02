{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.AccountLimit where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.AccountLimitType

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
--
--
-- /See:/ 'accountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { _alType :: !AccountLimitType,
    _alValue :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alType' - The limit that you requested. Valid values include the following:     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
--
-- * 'alValue' - The current value for the limit that is specified by <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type> .
accountLimit ::
  -- | 'alType'
  AccountLimitType ->
  -- | 'alValue'
  Natural ->
  AccountLimit
accountLimit pType_ pValue_ =
  AccountLimit' {_alType = pType_, _alValue = _Nat # pValue_}

-- | The limit that you requested. Valid values include the following:     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
alType :: Lens' AccountLimit AccountLimitType
alType = lens _alType (\s a -> s {_alType = a})

-- | The current value for the limit that is specified by <https://docs.aws.amazon.com/Route53/latest/APIReference/API_AccountLimit.html#Route53-Type-AccountLimit-Type Type> .
alValue :: Lens' AccountLimit Natural
alValue = lens _alValue (\s a -> s {_alValue = a}) . _Nat

instance FromXML AccountLimit where
  parseXML x = AccountLimit' <$> (x .@ "Type") <*> (x .@ "Value")

instance Hashable AccountLimit

instance NFData AccountLimit
