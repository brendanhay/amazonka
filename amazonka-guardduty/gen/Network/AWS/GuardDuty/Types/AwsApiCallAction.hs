{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the API action.
--
-- /See:/ 'newAwsApiCallAction' smart constructor.
data AwsApiCallAction = AwsApiCallAction'
  { -- | The AWS API name.
    api :: Prelude.Maybe Prelude.Text,
    -- | The AWS service name whose API was invoked.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The domain information for the AWS API call.
    domainDetails :: Prelude.Maybe DomainDetails,
    -- | The remote IP information of the connection that initiated the AWS API
    -- call.
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The AWS API caller type.
    callerType :: Prelude.Maybe Prelude.Text,
    -- | The error code of the failed AWS API action.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { api = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      domainDetails = Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing,
      callerType = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The AWS API name.
awsApiCallAction_api :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_api = Lens.lens (\AwsApiCallAction' {api} -> api) (\s@AwsApiCallAction' {} a -> s {api = a} :: AwsApiCallAction)

-- | The AWS service name whose API was invoked.
awsApiCallAction_serviceName :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_serviceName = Lens.lens (\AwsApiCallAction' {serviceName} -> serviceName) (\s@AwsApiCallAction' {} a -> s {serviceName = a} :: AwsApiCallAction)

-- | The domain information for the AWS API call.
awsApiCallAction_domainDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe DomainDetails)
awsApiCallAction_domainDetails = Lens.lens (\AwsApiCallAction' {domainDetails} -> domainDetails) (\s@AwsApiCallAction' {} a -> s {domainDetails = a} :: AwsApiCallAction)

-- | The remote IP information of the connection that initiated the AWS API
-- call.
awsApiCallAction_remoteIpDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe RemoteIpDetails)
awsApiCallAction_remoteIpDetails = Lens.lens (\AwsApiCallAction' {remoteIpDetails} -> remoteIpDetails) (\s@AwsApiCallAction' {} a -> s {remoteIpDetails = a} :: AwsApiCallAction)

-- | The AWS API caller type.
awsApiCallAction_callerType :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_callerType = Lens.lens (\AwsApiCallAction' {callerType} -> callerType) (\s@AwsApiCallAction' {} a -> s {callerType = a} :: AwsApiCallAction)

-- | The error code of the failed AWS API action.
awsApiCallAction_errorCode :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_errorCode = Lens.lens (\AwsApiCallAction' {errorCode} -> errorCode) (\s@AwsApiCallAction' {} a -> s {errorCode = a} :: AwsApiCallAction)

instance Prelude.FromJSON AwsApiCallAction where
  parseJSON =
    Prelude.withObject
      "AwsApiCallAction"
      ( \x ->
          AwsApiCallAction'
            Prelude.<$> (x Prelude..:? "api")
            Prelude.<*> (x Prelude..:? "serviceName")
            Prelude.<*> (x Prelude..:? "domainDetails")
            Prelude.<*> (x Prelude..:? "remoteIpDetails")
            Prelude.<*> (x Prelude..:? "callerType")
            Prelude.<*> (x Prelude..:? "errorCode")
      )

instance Prelude.Hashable AwsApiCallAction

instance Prelude.NFData AwsApiCallAction
