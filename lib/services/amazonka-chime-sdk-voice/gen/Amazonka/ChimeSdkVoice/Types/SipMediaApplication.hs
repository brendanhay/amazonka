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
-- Module      : Amazonka.ChimeSdkVoice.Types.SipMediaApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.SipMediaApplication where

import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationEndpoint
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newSipMediaApplication' smart constructor.
data SipMediaApplication = SipMediaApplication'
  { awsRegion :: Prelude.Maybe Prelude.Text,
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    endpoints :: Prelude.Maybe (Prelude.NonEmpty SipMediaApplicationEndpoint),
    name :: Prelude.Maybe Prelude.Text,
    sipMediaApplicationId :: Prelude.Maybe Prelude.Text,
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
-- 'awsRegion', 'sipMediaApplication_awsRegion' - Undocumented member.
--
-- 'createdTimestamp', 'sipMediaApplication_createdTimestamp' - Undocumented member.
--
-- 'endpoints', 'sipMediaApplication_endpoints' - Undocumented member.
--
-- 'name', 'sipMediaApplication_name' - Undocumented member.
--
-- 'sipMediaApplicationId', 'sipMediaApplication_sipMediaApplicationId' - Undocumented member.
--
-- 'updatedTimestamp', 'sipMediaApplication_updatedTimestamp' - Undocumented member.
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

-- | Undocumented member.
sipMediaApplication_awsRegion :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.Text)
sipMediaApplication_awsRegion = Lens.lens (\SipMediaApplication' {awsRegion} -> awsRegion) (\s@SipMediaApplication' {} a -> s {awsRegion = a} :: SipMediaApplication)

-- | Undocumented member.
sipMediaApplication_createdTimestamp :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.UTCTime)
sipMediaApplication_createdTimestamp = Lens.lens (\SipMediaApplication' {createdTimestamp} -> createdTimestamp) (\s@SipMediaApplication' {} a -> s {createdTimestamp = a} :: SipMediaApplication) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
sipMediaApplication_endpoints :: Lens.Lens' SipMediaApplication (Prelude.Maybe (Prelude.NonEmpty SipMediaApplicationEndpoint))
sipMediaApplication_endpoints = Lens.lens (\SipMediaApplication' {endpoints} -> endpoints) (\s@SipMediaApplication' {} a -> s {endpoints = a} :: SipMediaApplication) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
sipMediaApplication_name :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.Text)
sipMediaApplication_name = Lens.lens (\SipMediaApplication' {name} -> name) (\s@SipMediaApplication' {} a -> s {name = a} :: SipMediaApplication)

-- | Undocumented member.
sipMediaApplication_sipMediaApplicationId :: Lens.Lens' SipMediaApplication (Prelude.Maybe Prelude.Text)
sipMediaApplication_sipMediaApplicationId = Lens.lens (\SipMediaApplication' {sipMediaApplicationId} -> sipMediaApplicationId) (\s@SipMediaApplication' {} a -> s {sipMediaApplicationId = a} :: SipMediaApplication)

-- | Undocumented member.
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
    Prelude.rnf awsRegion `Prelude.seq`
      Prelude.rnf createdTimestamp `Prelude.seq`
        Prelude.rnf endpoints `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf sipMediaApplicationId `Prelude.seq`
              Prelude.rnf updatedTimestamp
