{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheck where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.CloudWatchAlarmConfiguration
import Network.AWS.Route53.Types.HealthCheckConfig
import Network.AWS.Route53.Types.LinkedService

-- | A complex type that contains information about one health check that is associated with the current AWS account.
--
--
--
-- /See:/ 'healthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { _hcLinkedService ::
      !(Maybe LinkedService),
    _hcCloudWatchAlarmConfiguration ::
      !(Maybe CloudWatchAlarmConfiguration),
    _hcId :: !Text,
    _hcCallerReference :: !Text,
    _hcHealthCheckConfig :: !HealthCheckConfig,
    _hcHealthCheckVersion :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcLinkedService' - If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- * 'hcCloudWatchAlarmConfiguration' - A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
-- * 'hcId' - The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
--
-- * 'hcCallerReference' - A unique string that you specified when you created the health check.
--
-- * 'hcHealthCheckConfig' - A complex type that contains detailed information about one health check.
--
-- * 'hcHealthCheckVersion' - The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
healthCheck ::
  -- | 'hcId'
  Text ->
  -- | 'hcCallerReference'
  Text ->
  -- | 'hcHealthCheckConfig'
  HealthCheckConfig ->
  -- | 'hcHealthCheckVersion'
  Natural ->
  HealthCheck
healthCheck
  pId_
  pCallerReference_
  pHealthCheckConfig_
  pHealthCheckVersion_ =
    HealthCheck'
      { _hcLinkedService = Nothing,
        _hcCloudWatchAlarmConfiguration = Nothing,
        _hcId = pId_,
        _hcCallerReference = pCallerReference_,
        _hcHealthCheckConfig = pHealthCheckConfig_,
        _hcHealthCheckVersion = _Nat # pHealthCheckVersion_
      }

-- | If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
hcLinkedService :: Lens' HealthCheck (Maybe LinkedService)
hcLinkedService = lens _hcLinkedService (\s a -> s {_hcLinkedService = a})

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
hcCloudWatchAlarmConfiguration :: Lens' HealthCheck (Maybe CloudWatchAlarmConfiguration)
hcCloudWatchAlarmConfiguration = lens _hcCloudWatchAlarmConfiguration (\s a -> s {_hcCloudWatchAlarmConfiguration = a})

-- | The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
hcId :: Lens' HealthCheck Text
hcId = lens _hcId (\s a -> s {_hcId = a})

-- | A unique string that you specified when you created the health check.
hcCallerReference :: Lens' HealthCheck Text
hcCallerReference = lens _hcCallerReference (\s a -> s {_hcCallerReference = a})

-- | A complex type that contains detailed information about one health check.
hcHealthCheckConfig :: Lens' HealthCheck HealthCheckConfig
hcHealthCheckConfig = lens _hcHealthCheckConfig (\s a -> s {_hcHealthCheckConfig = a})

-- | The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
hcHealthCheckVersion :: Lens' HealthCheck Natural
hcHealthCheckVersion = lens _hcHealthCheckVersion (\s a -> s {_hcHealthCheckVersion = a}) . _Nat

instance FromXML HealthCheck where
  parseXML x =
    HealthCheck'
      <$> (x .@? "LinkedService")
      <*> (x .@? "CloudWatchAlarmConfiguration")
      <*> (x .@ "Id")
      <*> (x .@ "CallerReference")
      <*> (x .@ "HealthCheckConfig")
      <*> (x .@ "HealthCheckVersion")

instance Hashable HealthCheck

instance NFData HealthCheck
