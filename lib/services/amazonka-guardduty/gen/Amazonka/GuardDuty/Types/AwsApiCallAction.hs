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
-- Module      : Amazonka.GuardDuty.Types.AwsApiCallAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.AwsApiCallAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DomainDetails
import Amazonka.GuardDuty.Types.RemoteAccountDetails
import Amazonka.GuardDuty.Types.RemoteIpDetails
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the API action.
--
-- /See:/ 'newAwsApiCallAction' smart constructor.
data AwsApiCallAction = AwsApiCallAction'
  { -- | The details of the Amazon Web Services account that made the API call.
    -- This field identifies the resources that were affected by this API call.
    affectedResources :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The remote IP information of the connection that initiated the Amazon
    -- Web Services API call.
    remoteIpDetails :: Prelude.Maybe RemoteIpDetails,
    -- | The details of the Amazon Web Services account that made the API call.
    -- This field appears if the call was made from outside your account.
    remoteAccountDetails :: Prelude.Maybe RemoteAccountDetails,
    -- | The domain information for the Amazon Web Services API call.
    domainDetails :: Prelude.Maybe DomainDetails,
    -- | The Amazon Web Services API name.
    api :: Prelude.Maybe Prelude.Text,
    -- | The error code of the failed Amazon Web Services API action.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services service name whose API was invoked.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The agent through which the API request was made.
    userAgent :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services API caller type.
    callerType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsApiCallAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affectedResources', 'awsApiCallAction_affectedResources' - The details of the Amazon Web Services account that made the API call.
-- This field identifies the resources that were affected by this API call.
--
-- 'remoteIpDetails', 'awsApiCallAction_remoteIpDetails' - The remote IP information of the connection that initiated the Amazon
-- Web Services API call.
--
-- 'remoteAccountDetails', 'awsApiCallAction_remoteAccountDetails' - The details of the Amazon Web Services account that made the API call.
-- This field appears if the call was made from outside your account.
--
-- 'domainDetails', 'awsApiCallAction_domainDetails' - The domain information for the Amazon Web Services API call.
--
-- 'api', 'awsApiCallAction_api' - The Amazon Web Services API name.
--
-- 'errorCode', 'awsApiCallAction_errorCode' - The error code of the failed Amazon Web Services API action.
--
-- 'serviceName', 'awsApiCallAction_serviceName' - The Amazon Web Services service name whose API was invoked.
--
-- 'userAgent', 'awsApiCallAction_userAgent' - The agent through which the API request was made.
--
-- 'callerType', 'awsApiCallAction_callerType' - The Amazon Web Services API caller type.
newAwsApiCallAction ::
  AwsApiCallAction
newAwsApiCallAction =
  AwsApiCallAction'
    { affectedResources =
        Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing,
      remoteAccountDetails = Prelude.Nothing,
      domainDetails = Prelude.Nothing,
      api = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      userAgent = Prelude.Nothing,
      callerType = Prelude.Nothing
    }

-- | The details of the Amazon Web Services account that made the API call.
-- This field identifies the resources that were affected by this API call.
awsApiCallAction_affectedResources :: Lens.Lens' AwsApiCallAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsApiCallAction_affectedResources = Lens.lens (\AwsApiCallAction' {affectedResources} -> affectedResources) (\s@AwsApiCallAction' {} a -> s {affectedResources = a} :: AwsApiCallAction) Prelude.. Lens.mapping Lens.coerced

-- | The remote IP information of the connection that initiated the Amazon
-- Web Services API call.
awsApiCallAction_remoteIpDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe RemoteIpDetails)
awsApiCallAction_remoteIpDetails = Lens.lens (\AwsApiCallAction' {remoteIpDetails} -> remoteIpDetails) (\s@AwsApiCallAction' {} a -> s {remoteIpDetails = a} :: AwsApiCallAction)

-- | The details of the Amazon Web Services account that made the API call.
-- This field appears if the call was made from outside your account.
awsApiCallAction_remoteAccountDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe RemoteAccountDetails)
awsApiCallAction_remoteAccountDetails = Lens.lens (\AwsApiCallAction' {remoteAccountDetails} -> remoteAccountDetails) (\s@AwsApiCallAction' {} a -> s {remoteAccountDetails = a} :: AwsApiCallAction)

-- | The domain information for the Amazon Web Services API call.
awsApiCallAction_domainDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe DomainDetails)
awsApiCallAction_domainDetails = Lens.lens (\AwsApiCallAction' {domainDetails} -> domainDetails) (\s@AwsApiCallAction' {} a -> s {domainDetails = a} :: AwsApiCallAction)

-- | The Amazon Web Services API name.
awsApiCallAction_api :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_api = Lens.lens (\AwsApiCallAction' {api} -> api) (\s@AwsApiCallAction' {} a -> s {api = a} :: AwsApiCallAction)

-- | The error code of the failed Amazon Web Services API action.
awsApiCallAction_errorCode :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_errorCode = Lens.lens (\AwsApiCallAction' {errorCode} -> errorCode) (\s@AwsApiCallAction' {} a -> s {errorCode = a} :: AwsApiCallAction)

-- | The Amazon Web Services service name whose API was invoked.
awsApiCallAction_serviceName :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_serviceName = Lens.lens (\AwsApiCallAction' {serviceName} -> serviceName) (\s@AwsApiCallAction' {} a -> s {serviceName = a} :: AwsApiCallAction)

-- | The agent through which the API request was made.
awsApiCallAction_userAgent :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_userAgent = Lens.lens (\AwsApiCallAction' {userAgent} -> userAgent) (\s@AwsApiCallAction' {} a -> s {userAgent = a} :: AwsApiCallAction)

-- | The Amazon Web Services API caller type.
awsApiCallAction_callerType :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_callerType = Lens.lens (\AwsApiCallAction' {callerType} -> callerType) (\s@AwsApiCallAction' {} a -> s {callerType = a} :: AwsApiCallAction)

instance Data.FromJSON AwsApiCallAction where
  parseJSON =
    Data.withObject
      "AwsApiCallAction"
      ( \x ->
          AwsApiCallAction'
            Prelude.<$> ( x Data..:? "affectedResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "remoteIpDetails")
            Prelude.<*> (x Data..:? "remoteAccountDetails")
            Prelude.<*> (x Data..:? "domainDetails")
            Prelude.<*> (x Data..:? "api")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "serviceName")
            Prelude.<*> (x Data..:? "userAgent")
            Prelude.<*> (x Data..:? "callerType")
      )

instance Prelude.Hashable AwsApiCallAction where
  hashWithSalt _salt AwsApiCallAction' {..} =
    _salt `Prelude.hashWithSalt` affectedResources
      `Prelude.hashWithSalt` remoteIpDetails
      `Prelude.hashWithSalt` remoteAccountDetails
      `Prelude.hashWithSalt` domainDetails
      `Prelude.hashWithSalt` api
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` userAgent
      `Prelude.hashWithSalt` callerType

instance Prelude.NFData AwsApiCallAction where
  rnf AwsApiCallAction' {..} =
    Prelude.rnf affectedResources
      `Prelude.seq` Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf remoteAccountDetails
      `Prelude.seq` Prelude.rnf domainDetails
      `Prelude.seq` Prelude.rnf api
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf userAgent
      `Prelude.seq` Prelude.rnf callerType
