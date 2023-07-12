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
-- Module      : Amazonka.Chime.Types.SipMediaApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.SipMediaApplication where

import Amazonka.Chime.Types.SipMediaApplicationEndpoint
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the SIP media application, including name and endpoints.
-- An AWS account can have multiple SIP media applications.
--
-- /See:/ 'newSipMediaApplication' smart constructor.
data SipMediaApplication = SipMediaApplication'
  { -- | The AWS Region in which the SIP media application is created.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The SIP media application creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | List of endpoints for SIP media application. Currently, only one
    -- endpoint per SIP media application is permitted.
    endpoints :: Prelude.Maybe (Prelude.NonEmpty SipMediaApplicationEndpoint),
    -- | The name of the SIP media application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The SIP media application ID.
    sipMediaApplicationId :: Prelude.Maybe Prelude.Text,
    -- | The SIP media application updated timestamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SipMediaApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsRegion', 'sipMediaApplication_awsRegion' - The AWS Region in which the SIP media application is created.
--
-- 'createdTimestamp', 'sipMediaApplication_createdTimestamp' - The SIP media application creation timestamp, in ISO 8601 format.
--
-- 'endpoints', 'sipMediaApplication_endpoints' - List of endpoints for SIP media application. Currently, only one
-- endpoint per SIP media application is permitted.
--
-- 'name', 'sipMediaApplication_name' - The name of the SIP media application.
--
-- 'sipMediaApplicationId', 'sipMediaApplication_sipMediaApplicationId' - The SIP media application ID.
--
-- 'updatedTimestamp', 'sipMediaApplication_updatedTimestamp' - The SIP media application updated timestamp, in ISO 8601 format.
newSipMediaApplication ::
  SipMediaApplication
newSipMediaApplication =
  SipMediaApplication'
    { awsRegion = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      name = Prelude.Nothing,
      sipMediaApplicationId = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The AWS Region in which the SIP media application is created.
sipMediaApplication_awsRegion :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.Text)
sipMediaApplication_awsRegion = Lens.lens (\SipMediaApplication' {awsRegion} -> awsRegion) (\s@SipMediaApplication' {} a -> s {awsRegion = a} :: SipMediaApplication)

-- | The SIP media application creation timestamp, in ISO 8601 format.
sipMediaApplication_createdTimestamp :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.UTCTime)
sipMediaApplication_createdTimestamp = Lens.lens (\SipMediaApplication' {createdTimestamp} -> createdTimestamp) (\s@SipMediaApplication' {} a -> s {createdTimestamp = a} :: SipMediaApplication) Prelude.. Lens.mapping Data._Time

-- | List of endpoints for SIP media application. Currently, only one
-- endpoint per SIP media application is permitted.
sipMediaApplication_endpoints :: Lens.Lens' SipMediaApplication (Prelude.Maybe (Prelude.NonEmpty SipMediaApplicationEndpoint))
sipMediaApplication_endpoints = Lens.lens (\SipMediaApplication' {endpoints} -> endpoints) (\s@SipMediaApplication' {} a -> s {endpoints = a} :: SipMediaApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the SIP media application.
sipMediaApplication_name :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.Text)
sipMediaApplication_name = Lens.lens (\SipMediaApplication' {name} -> name) (\s@SipMediaApplication' {} a -> s {name = a} :: SipMediaApplication)

-- | The SIP media application ID.
sipMediaApplication_sipMediaApplicationId :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.Text)
sipMediaApplication_sipMediaApplicationId = Lens.lens (\SipMediaApplication' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@SipMediaApplication' {} a -> s {sipMediaApplicationId = a} :: SipMediaApplication)

-- | The SIP media application updated timestamp, in ISO 8601 format.
sipMediaApplication_updatedTimestamp :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.UTCTime)
sipMediaApplication_updatedTimestamp = Lens.lens (\SipMediaApplication' {updatedTimestamp} -> updatedTimestamp) (\s@SipMediaApplication' {} a -> s {updatedTimestamp = a} :: SipMediaApplication) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON SipMediaApplication where
  parseJSON =
    Data.withObject
      "SipMediaApplication"
      ( \x ->
          SipMediaApplication'
            Prelude.<$> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Endpoints")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "SipMediaApplicationId")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance Prelude.Hashable SipMediaApplication where
  hashWithSalt _salt SipMediaApplication' {..} =
    _salt
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sipMediaApplicationId
      `Prelude.hashWithSalt` updatedTimestamp

instance Prelude.NFData SipMediaApplication where
  rnf SipMediaApplication' {..} =
    Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sipMediaApplicationId
      `Prelude.seq` Prelude.rnf updatedTimestamp
