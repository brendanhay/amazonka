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
-- Module      : Amazonka.AppFlow.Types.SAPODataConnectorProfileProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SAPODataConnectorProfileProperties where

import Amazonka.AppFlow.Types.OAuthProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using SAPOData.
--
-- /See:/ 'newSAPODataConnectorProfileProperties' smart constructor.
data SAPODataConnectorProfileProperties = SAPODataConnectorProfileProperties'
  { -- | The logon language of SAPOData instance.
    logonLanguage :: Prelude.Maybe Prelude.Text,
    -- | The SAPOData OAuth properties required for OAuth type authentication.
    oAuthProperties :: Prelude.Maybe OAuthProperties,
    -- | The SAPOData Private Link service name to be used for private data
    -- transfers.
    privateLinkServiceName :: Prelude.Maybe Prelude.Text,
    -- | The location of the SAPOData resource.
    applicationHostUrl :: Prelude.Text,
    -- | The application path to catalog service.
    applicationServicePath :: Prelude.Text,
    -- | The port number of the SAPOData instance.
    portNumber :: Prelude.Natural,
    -- | The client number for the client creating the connection.
    clientNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAPODataConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logonLanguage', 'sAPODataConnectorProfileProperties_logonLanguage' - The logon language of SAPOData instance.
--
-- 'oAuthProperties', 'sAPODataConnectorProfileProperties_oAuthProperties' - The SAPOData OAuth properties required for OAuth type authentication.
--
-- 'privateLinkServiceName', 'sAPODataConnectorProfileProperties_privateLinkServiceName' - The SAPOData Private Link service name to be used for private data
-- transfers.
--
-- 'applicationHostUrl', 'sAPODataConnectorProfileProperties_applicationHostUrl' - The location of the SAPOData resource.
--
-- 'applicationServicePath', 'sAPODataConnectorProfileProperties_applicationServicePath' - The application path to catalog service.
--
-- 'portNumber', 'sAPODataConnectorProfileProperties_portNumber' - The port number of the SAPOData instance.
--
-- 'clientNumber', 'sAPODataConnectorProfileProperties_clientNumber' - The client number for the client creating the connection.
newSAPODataConnectorProfileProperties ::
  -- | 'applicationHostUrl'
  Prelude.Text ->
  -- | 'applicationServicePath'
  Prelude.Text ->
  -- | 'portNumber'
  Prelude.Natural ->
  -- | 'clientNumber'
  Prelude.Text ->
  SAPODataConnectorProfileProperties
newSAPODataConnectorProfileProperties
  pApplicationHostUrl_
  pApplicationServicePath_
  pPortNumber_
  pClientNumber_ =
    SAPODataConnectorProfileProperties'
      { logonLanguage =
          Prelude.Nothing,
        oAuthProperties = Prelude.Nothing,
        privateLinkServiceName =
          Prelude.Nothing,
        applicationHostUrl =
          pApplicationHostUrl_,
        applicationServicePath =
          pApplicationServicePath_,
        portNumber = pPortNumber_,
        clientNumber = pClientNumber_
      }

-- | The logon language of SAPOData instance.
sAPODataConnectorProfileProperties_logonLanguage :: Lens.Lens' SAPODataConnectorProfileProperties (Prelude.Maybe Prelude.Text)
sAPODataConnectorProfileProperties_logonLanguage = Lens.lens (\SAPODataConnectorProfileProperties' {logonLanguage} -> logonLanguage) (\s@SAPODataConnectorProfileProperties' {} a -> s {logonLanguage = a} :: SAPODataConnectorProfileProperties)

-- | The SAPOData OAuth properties required for OAuth type authentication.
sAPODataConnectorProfileProperties_oAuthProperties :: Lens.Lens' SAPODataConnectorProfileProperties (Prelude.Maybe OAuthProperties)
sAPODataConnectorProfileProperties_oAuthProperties = Lens.lens (\SAPODataConnectorProfileProperties' {oAuthProperties} -> oAuthProperties) (\s@SAPODataConnectorProfileProperties' {} a -> s {oAuthProperties = a} :: SAPODataConnectorProfileProperties)

-- | The SAPOData Private Link service name to be used for private data
-- transfers.
sAPODataConnectorProfileProperties_privateLinkServiceName :: Lens.Lens' SAPODataConnectorProfileProperties (Prelude.Maybe Prelude.Text)
sAPODataConnectorProfileProperties_privateLinkServiceName = Lens.lens (\SAPODataConnectorProfileProperties' {privateLinkServiceName} -> privateLinkServiceName) (\s@SAPODataConnectorProfileProperties' {} a -> s {privateLinkServiceName = a} :: SAPODataConnectorProfileProperties)

-- | The location of the SAPOData resource.
sAPODataConnectorProfileProperties_applicationHostUrl :: Lens.Lens' SAPODataConnectorProfileProperties Prelude.Text
sAPODataConnectorProfileProperties_applicationHostUrl = Lens.lens (\SAPODataConnectorProfileProperties' {applicationHostUrl} -> applicationHostUrl) (\s@SAPODataConnectorProfileProperties' {} a -> s {applicationHostUrl = a} :: SAPODataConnectorProfileProperties)

-- | The application path to catalog service.
sAPODataConnectorProfileProperties_applicationServicePath :: Lens.Lens' SAPODataConnectorProfileProperties Prelude.Text
sAPODataConnectorProfileProperties_applicationServicePath = Lens.lens (\SAPODataConnectorProfileProperties' {applicationServicePath} -> applicationServicePath) (\s@SAPODataConnectorProfileProperties' {} a -> s {applicationServicePath = a} :: SAPODataConnectorProfileProperties)

-- | The port number of the SAPOData instance.
sAPODataConnectorProfileProperties_portNumber :: Lens.Lens' SAPODataConnectorProfileProperties Prelude.Natural
sAPODataConnectorProfileProperties_portNumber = Lens.lens (\SAPODataConnectorProfileProperties' {portNumber} -> portNumber) (\s@SAPODataConnectorProfileProperties' {} a -> s {portNumber = a} :: SAPODataConnectorProfileProperties)

-- | The client number for the client creating the connection.
sAPODataConnectorProfileProperties_clientNumber :: Lens.Lens' SAPODataConnectorProfileProperties Prelude.Text
sAPODataConnectorProfileProperties_clientNumber = Lens.lens (\SAPODataConnectorProfileProperties' {clientNumber} -> clientNumber) (\s@SAPODataConnectorProfileProperties' {} a -> s {clientNumber = a} :: SAPODataConnectorProfileProperties)

instance
  Data.FromJSON
    SAPODataConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "SAPODataConnectorProfileProperties"
      ( \x ->
          SAPODataConnectorProfileProperties'
            Prelude.<$> (x Data..:? "logonLanguage")
            Prelude.<*> (x Data..:? "oAuthProperties")
            Prelude.<*> (x Data..:? "privateLinkServiceName")
            Prelude.<*> (x Data..: "applicationHostUrl")
            Prelude.<*> (x Data..: "applicationServicePath")
            Prelude.<*> (x Data..: "portNumber")
            Prelude.<*> (x Data..: "clientNumber")
      )

instance
  Prelude.Hashable
    SAPODataConnectorProfileProperties
  where
  hashWithSalt
    _salt
    SAPODataConnectorProfileProperties' {..} =
      _salt
        `Prelude.hashWithSalt` logonLanguage
        `Prelude.hashWithSalt` oAuthProperties
        `Prelude.hashWithSalt` privateLinkServiceName
        `Prelude.hashWithSalt` applicationHostUrl
        `Prelude.hashWithSalt` applicationServicePath
        `Prelude.hashWithSalt` portNumber
        `Prelude.hashWithSalt` clientNumber

instance
  Prelude.NFData
    SAPODataConnectorProfileProperties
  where
  rnf SAPODataConnectorProfileProperties' {..} =
    Prelude.rnf logonLanguage
      `Prelude.seq` Prelude.rnf oAuthProperties
      `Prelude.seq` Prelude.rnf privateLinkServiceName
      `Prelude.seq` Prelude.rnf applicationHostUrl
      `Prelude.seq` Prelude.rnf applicationServicePath
      `Prelude.seq` Prelude.rnf portNumber
      `Prelude.seq` Prelude.rnf clientNumber

instance
  Data.ToJSON
    SAPODataConnectorProfileProperties
  where
  toJSON SAPODataConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logonLanguage" Data..=) Prelude.<$> logonLanguage,
            ("oAuthProperties" Data..=)
              Prelude.<$> oAuthProperties,
            ("privateLinkServiceName" Data..=)
              Prelude.<$> privateLinkServiceName,
            Prelude.Just
              ("applicationHostUrl" Data..= applicationHostUrl),
            Prelude.Just
              ( "applicationServicePath"
                  Data..= applicationServicePath
              ),
            Prelude.Just ("portNumber" Data..= portNumber),
            Prelude.Just ("clientNumber" Data..= clientNumber)
          ]
      )
