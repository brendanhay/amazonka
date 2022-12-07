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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainMasterUserOptionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainMasterUserOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies information about the master user of the domain.
--
-- /See:/ 'newAwsOpenSearchServiceDomainMasterUserOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainMasterUserOptionsDetails = AwsOpenSearchServiceDomainMasterUserOptionsDetails'
  { -- | The Amazon Resource Name (ARN) for the master user.
    masterUserArn :: Prelude.Maybe Prelude.Text,
    -- | The username for the master user.
    masterUserName :: Prelude.Maybe Prelude.Text,
    -- | The password for the master user.
    masterUserPassword :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainMasterUserOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterUserArn', 'awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserArn' - The Amazon Resource Name (ARN) for the master user.
--
-- 'masterUserName', 'awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserName' - The username for the master user.
--
-- 'masterUserPassword', 'awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserPassword' - The password for the master user.
newAwsOpenSearchServiceDomainMasterUserOptionsDetails ::
  AwsOpenSearchServiceDomainMasterUserOptionsDetails
newAwsOpenSearchServiceDomainMasterUserOptionsDetails =
  AwsOpenSearchServiceDomainMasterUserOptionsDetails'
    { masterUserArn =
        Prelude.Nothing,
      masterUserName =
        Prelude.Nothing,
      masterUserPassword =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the master user.
awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserArn :: Lens.Lens' AwsOpenSearchServiceDomainMasterUserOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserArn = Lens.lens (\AwsOpenSearchServiceDomainMasterUserOptionsDetails' {masterUserArn} -> masterUserArn) (\s@AwsOpenSearchServiceDomainMasterUserOptionsDetails' {} a -> s {masterUserArn = a} :: AwsOpenSearchServiceDomainMasterUserOptionsDetails)

-- | The username for the master user.
awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserName :: Lens.Lens' AwsOpenSearchServiceDomainMasterUserOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserName = Lens.lens (\AwsOpenSearchServiceDomainMasterUserOptionsDetails' {masterUserName} -> masterUserName) (\s@AwsOpenSearchServiceDomainMasterUserOptionsDetails' {} a -> s {masterUserName = a} :: AwsOpenSearchServiceDomainMasterUserOptionsDetails)

-- | The password for the master user.
awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserPassword :: Lens.Lens' AwsOpenSearchServiceDomainMasterUserOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainMasterUserOptionsDetails_masterUserPassword = Lens.lens (\AwsOpenSearchServiceDomainMasterUserOptionsDetails' {masterUserPassword} -> masterUserPassword) (\s@AwsOpenSearchServiceDomainMasterUserOptionsDetails' {} a -> s {masterUserPassword = a} :: AwsOpenSearchServiceDomainMasterUserOptionsDetails)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainMasterUserOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainMasterUserOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainMasterUserOptionsDetails'
            Prelude.<$> (x Data..:? "MasterUserArn")
              Prelude.<*> (x Data..:? "MasterUserName")
              Prelude.<*> (x Data..:? "MasterUserPassword")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainMasterUserOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainMasterUserOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` masterUserArn
        `Prelude.hashWithSalt` masterUserName
        `Prelude.hashWithSalt` masterUserPassword

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainMasterUserOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainMasterUserOptionsDetails' {..} =
      Prelude.rnf masterUserArn
        `Prelude.seq` Prelude.rnf masterUserName
        `Prelude.seq` Prelude.rnf masterUserPassword

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainMasterUserOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainMasterUserOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("MasterUserArn" Data..=) Prelude.<$> masterUserArn,
              ("MasterUserName" Data..=)
                Prelude.<$> masterUserName,
              ("MasterUserPassword" Data..=)
                Prelude.<$> masterUserPassword
            ]
        )
