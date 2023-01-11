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
-- Module      : Amazonka.SecurityHub.Types.AwsApiCallAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsApiCallAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ActionRemoteIpDetails
import Amazonka.SecurityHub.Types.AwsApiCallActionDomainDetails

-- | Provided if @ActionType@ is @AWS_API_CALL@. It provides details about
-- the API call that was detected.
--
-- /See:/ 'newAwsApiCallAction' smart constructor.
data AwsApiCallAction = AwsApiCallAction'
  { -- | Identifies the resources that were affected by the API call.
    affectedResources :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the API method that was issued.
    api :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the API call originated from a remote IP address
    -- (@remoteip@) or from a DNS domain (@domain@).
    callerType :: Prelude.Maybe Prelude.Text,
    -- | Provided if @CallerType@ is @domain@. Provides information about the DNS
    -- domain that the API call originated from.
    domainDetails :: Prelude.Maybe AwsApiCallActionDomainDetails,
    -- | An ISO8601-formatted timestamp that indicates when the API call was
    -- first observed.
    firstSeen :: Prelude.Maybe Prelude.Text,
    -- | An ISO8601-formatted timestamp that indicates when the API call was most
    -- recently observed.
    lastSeen :: Prelude.Maybe Prelude.Text,
    -- | Provided if @CallerType@ is @remoteIp@. Provides information about the
    -- remote IP address that the API call originated from.
    remoteIpDetails :: Prelude.Maybe ActionRemoteIpDetails,
    -- | The name of the Amazon Web Services service that the API method belongs
    -- to.
    serviceName :: Prelude.Maybe Prelude.Text
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
-- 'affectedResources', 'awsApiCallAction_affectedResources' - Identifies the resources that were affected by the API call.
--
-- 'api', 'awsApiCallAction_api' - The name of the API method that was issued.
--
-- 'callerType', 'awsApiCallAction_callerType' - Indicates whether the API call originated from a remote IP address
-- (@remoteip@) or from a DNS domain (@domain@).
--
-- 'domainDetails', 'awsApiCallAction_domainDetails' - Provided if @CallerType@ is @domain@. Provides information about the DNS
-- domain that the API call originated from.
--
-- 'firstSeen', 'awsApiCallAction_firstSeen' - An ISO8601-formatted timestamp that indicates when the API call was
-- first observed.
--
-- 'lastSeen', 'awsApiCallAction_lastSeen' - An ISO8601-formatted timestamp that indicates when the API call was most
-- recently observed.
--
-- 'remoteIpDetails', 'awsApiCallAction_remoteIpDetails' - Provided if @CallerType@ is @remoteIp@. Provides information about the
-- remote IP address that the API call originated from.
--
-- 'serviceName', 'awsApiCallAction_serviceName' - The name of the Amazon Web Services service that the API method belongs
-- to.
newAwsApiCallAction ::
  AwsApiCallAction
newAwsApiCallAction =
  AwsApiCallAction'
    { affectedResources =
        Prelude.Nothing,
      api = Prelude.Nothing,
      callerType = Prelude.Nothing,
      domainDetails = Prelude.Nothing,
      firstSeen = Prelude.Nothing,
      lastSeen = Prelude.Nothing,
      remoteIpDetails = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | Identifies the resources that were affected by the API call.
awsApiCallAction_affectedResources :: Lens.Lens' AwsApiCallAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsApiCallAction_affectedResources = Lens.lens (\AwsApiCallAction' {affectedResources} -> affectedResources) (\s@AwsApiCallAction' {} a -> s {affectedResources = a} :: AwsApiCallAction) Prelude.. Lens.mapping Lens.coerced

-- | The name of the API method that was issued.
awsApiCallAction_api :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_api = Lens.lens (\AwsApiCallAction' {api} -> api) (\s@AwsApiCallAction' {} a -> s {api = a} :: AwsApiCallAction)

-- | Indicates whether the API call originated from a remote IP address
-- (@remoteip@) or from a DNS domain (@domain@).
awsApiCallAction_callerType :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_callerType = Lens.lens (\AwsApiCallAction' {callerType} -> callerType) (\s@AwsApiCallAction' {} a -> s {callerType = a} :: AwsApiCallAction)

-- | Provided if @CallerType@ is @domain@. Provides information about the DNS
-- domain that the API call originated from.
awsApiCallAction_domainDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe AwsApiCallActionDomainDetails)
awsApiCallAction_domainDetails = Lens.lens (\AwsApiCallAction' {domainDetails} -> domainDetails) (\s@AwsApiCallAction' {} a -> s {domainDetails = a} :: AwsApiCallAction)

-- | An ISO8601-formatted timestamp that indicates when the API call was
-- first observed.
awsApiCallAction_firstSeen :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_firstSeen = Lens.lens (\AwsApiCallAction' {firstSeen} -> firstSeen) (\s@AwsApiCallAction' {} a -> s {firstSeen = a} :: AwsApiCallAction)

-- | An ISO8601-formatted timestamp that indicates when the API call was most
-- recently observed.
awsApiCallAction_lastSeen :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_lastSeen = Lens.lens (\AwsApiCallAction' {lastSeen} -> lastSeen) (\s@AwsApiCallAction' {} a -> s {lastSeen = a} :: AwsApiCallAction)

-- | Provided if @CallerType@ is @remoteIp@. Provides information about the
-- remote IP address that the API call originated from.
awsApiCallAction_remoteIpDetails :: Lens.Lens' AwsApiCallAction (Prelude.Maybe ActionRemoteIpDetails)
awsApiCallAction_remoteIpDetails = Lens.lens (\AwsApiCallAction' {remoteIpDetails} -> remoteIpDetails) (\s@AwsApiCallAction' {} a -> s {remoteIpDetails = a} :: AwsApiCallAction)

-- | The name of the Amazon Web Services service that the API method belongs
-- to.
awsApiCallAction_serviceName :: Lens.Lens' AwsApiCallAction (Prelude.Maybe Prelude.Text)
awsApiCallAction_serviceName = Lens.lens (\AwsApiCallAction' {serviceName} -> serviceName) (\s@AwsApiCallAction' {} a -> s {serviceName = a} :: AwsApiCallAction)

instance Data.FromJSON AwsApiCallAction where
  parseJSON =
    Data.withObject
      "AwsApiCallAction"
      ( \x ->
          AwsApiCallAction'
            Prelude.<$> ( x Data..:? "AffectedResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Api")
            Prelude.<*> (x Data..:? "CallerType")
            Prelude.<*> (x Data..:? "DomainDetails")
            Prelude.<*> (x Data..:? "FirstSeen")
            Prelude.<*> (x Data..:? "LastSeen")
            Prelude.<*> (x Data..:? "RemoteIpDetails")
            Prelude.<*> (x Data..:? "ServiceName")
      )

instance Prelude.Hashable AwsApiCallAction where
  hashWithSalt _salt AwsApiCallAction' {..} =
    _salt `Prelude.hashWithSalt` affectedResources
      `Prelude.hashWithSalt` api
      `Prelude.hashWithSalt` callerType
      `Prelude.hashWithSalt` domainDetails
      `Prelude.hashWithSalt` firstSeen
      `Prelude.hashWithSalt` lastSeen
      `Prelude.hashWithSalt` remoteIpDetails
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData AwsApiCallAction where
  rnf AwsApiCallAction' {..} =
    Prelude.rnf affectedResources
      `Prelude.seq` Prelude.rnf api
      `Prelude.seq` Prelude.rnf callerType
      `Prelude.seq` Prelude.rnf domainDetails
      `Prelude.seq` Prelude.rnf firstSeen
      `Prelude.seq` Prelude.rnf lastSeen
      `Prelude.seq` Prelude.rnf remoteIpDetails
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToJSON AwsApiCallAction where
  toJSON AwsApiCallAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AffectedResources" Data..=)
              Prelude.<$> affectedResources,
            ("Api" Data..=) Prelude.<$> api,
            ("CallerType" Data..=) Prelude.<$> callerType,
            ("DomainDetails" Data..=) Prelude.<$> domainDetails,
            ("FirstSeen" Data..=) Prelude.<$> firstSeen,
            ("LastSeen" Data..=) Prelude.<$> lastSeen,
            ("RemoteIpDetails" Data..=)
              Prelude.<$> remoteIpDetails,
            ("ServiceName" Data..=) Prelude.<$> serviceName
          ]
      )
