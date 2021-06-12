{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AwsApiCallAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AwsApiCallAction where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import qualified Network.AWS.Lens as Lens

-- | Contains information about the API action.
--
-- /See:/ 'newAwsApiCallAction' smart constructor.
data AwsApiCallAction = AwsApiCallAction'
  { -- | The AWS API name.
    api :: Core.Maybe Core.Text,
    -- | The AWS service name whose API was invoked.
    serviceName :: Core.Maybe Core.Text,
    -- | The domain information for the AWS API call.
    domainDetails :: Core.Maybe DomainDetails,
    -- | The remote IP information of the connection that initiated the AWS API
    -- call.
    remoteIpDetails :: Core.Maybe RemoteIpDetails,
    -- | The AWS API caller type.
    callerType :: Core.Maybe Core.Text,
    -- | The error code of the failed AWS API action.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AwsApiCallAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'api', 'awsApiCallAction_api' - The AWS API name.
--
-- 'serviceName', 'awsApiCallAction_serviceName' - The AWS service name whose API was invoked.
--
-- 'domainDetails', 'awsApiCallAction_domainDetails' - The domain information for the AWS API call.
--
-- 'remoteIpDetails', 'awsApiCallAction_remoteIpDetails' - The remote IP information of the connection that initiated the AWS API
-- call.
--
-- 'callerType', 'awsApiCallAction_callerType' - The AWS API caller type.
--
-- 'errorCode', 'awsApiCallAction_errorCode' - The error code of the failed AWS API action.
newAwsApiCallAction ::
  AwsApiCallAction
newAwsApiCallAction =
  AwsApiCallAction'
    { api = Core.Nothing,
      serviceName = Core.Nothing,
      domainDetails = Core.Nothing,
      remoteIpDetails = Core.Nothing,
      callerType = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The AWS API name.
awsApiCallAction_api :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
awsApiCallAction_api = Lens.lens (\AwsApiCallAction' {api} -> api) (\s@AwsApiCallAction' {} a -> s {api = a} :: AwsApiCallAction)

-- | The AWS service name whose API was invoked.
awsApiCallAction_serviceName :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
awsApiCallAction_serviceName = Lens.lens (\AwsApiCallAction' {serviceName} -> serviceName) (\s@AwsApiCallAction' {} a -> s {serviceName = a} :: AwsApiCallAction)

-- | The domain information for the AWS API call.
awsApiCallAction_domainDetails :: Lens.Lens' AwsApiCallAction (Core.Maybe DomainDetails)
awsApiCallAction_domainDetails = Lens.lens (\AwsApiCallAction' {domainDetails} -> domainDetails) (\s@AwsApiCallAction' {} a -> s {domainDetails = a} :: AwsApiCallAction)

-- | The remote IP information of the connection that initiated the AWS API
-- call.
awsApiCallAction_remoteIpDetails :: Lens.Lens' AwsApiCallAction (Core.Maybe RemoteIpDetails)
awsApiCallAction_remoteIpDetails = Lens.lens (\AwsApiCallAction' {remoteIpDetails} -> remoteIpDetails) (\s@AwsApiCallAction' {} a -> s {remoteIpDetails = a} :: AwsApiCallAction)

-- | The AWS API caller type.
awsApiCallAction_callerType :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
awsApiCallAction_callerType = Lens.lens (\AwsApiCallAction' {callerType} -> callerType) (\s@AwsApiCallAction' {} a -> s {callerType = a} :: AwsApiCallAction)

-- | The error code of the failed AWS API action.
awsApiCallAction_errorCode :: Lens.Lens' AwsApiCallAction (Core.Maybe Core.Text)
awsApiCallAction_errorCode = Lens.lens (\AwsApiCallAction' {errorCode} -> errorCode) (\s@AwsApiCallAction' {} a -> s {errorCode = a} :: AwsApiCallAction)

instance Core.FromJSON AwsApiCallAction where
  parseJSON =
    Core.withObject
      "AwsApiCallAction"
      ( \x ->
          AwsApiCallAction'
            Core.<$> (x Core..:? "api")
            Core.<*> (x Core..:? "serviceName")
            Core.<*> (x Core..:? "domainDetails")
            Core.<*> (x Core..:? "remoteIpDetails")
            Core.<*> (x Core..:? "callerType")
            Core.<*> (x Core..:? "errorCode")
      )

instance Core.Hashable AwsApiCallAction

instance Core.NFData AwsApiCallAction
