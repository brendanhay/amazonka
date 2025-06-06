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
-- Module      : Amazonka.Redshift.Types.HsmStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.HsmStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes the status of changes to HSM settings.
--
-- /See:/ 'newHsmStatus' smart constructor.
data HsmStatus = HsmStatus'
  { -- | Specifies the name of the HSM client certificate the Amazon Redshift
    -- cluster uses to retrieve the data encryption keys stored in an HSM.
    hsmClientCertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the HSM configuration that contains the
    -- information the Amazon Redshift cluster can use to retrieve and store
    -- keys in an HSM.
    hsmConfigurationIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Reports whether the Amazon Redshift cluster has finished applying any
    -- HSM settings changes specified in a modify cluster command.
    --
    -- Values: active, applying
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HsmStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmClientCertificateIdentifier', 'hsmStatus_hsmClientCertificateIdentifier' - Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
--
-- 'hsmConfigurationIdentifier', 'hsmStatus_hsmConfigurationIdentifier' - Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
--
-- 'status', 'hsmStatus_status' - Reports whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
newHsmStatus ::
  HsmStatus
newHsmStatus =
  HsmStatus'
    { hsmClientCertificateIdentifier =
        Prelude.Nothing,
      hsmConfigurationIdentifier = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
hsmStatus_hsmClientCertificateIdentifier :: Lens.Lens' HsmStatus (Prelude.Maybe Prelude.Text)
hsmStatus_hsmClientCertificateIdentifier = Lens.lens (\HsmStatus' {hsmClientCertificateIdentifier} -> hsmClientCertificateIdentifier) (\s@HsmStatus' {} a -> s {hsmClientCertificateIdentifier = a} :: HsmStatus)

-- | Specifies the name of the HSM configuration that contains the
-- information the Amazon Redshift cluster can use to retrieve and store
-- keys in an HSM.
hsmStatus_hsmConfigurationIdentifier :: Lens.Lens' HsmStatus (Prelude.Maybe Prelude.Text)
hsmStatus_hsmConfigurationIdentifier = Lens.lens (\HsmStatus' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@HsmStatus' {} a -> s {hsmConfigurationIdentifier = a} :: HsmStatus)

-- | Reports whether the Amazon Redshift cluster has finished applying any
-- HSM settings changes specified in a modify cluster command.
--
-- Values: active, applying
hsmStatus_status :: Lens.Lens' HsmStatus (Prelude.Maybe Prelude.Text)
hsmStatus_status = Lens.lens (\HsmStatus' {status} -> status) (\s@HsmStatus' {} a -> s {status = a} :: HsmStatus)

instance Data.FromXML HsmStatus where
  parseXML x =
    HsmStatus'
      Prelude.<$> (x Data..@? "HsmClientCertificateIdentifier")
      Prelude.<*> (x Data..@? "HsmConfigurationIdentifier")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable HsmStatus where
  hashWithSalt _salt HsmStatus' {..} =
    _salt
      `Prelude.hashWithSalt` hsmClientCertificateIdentifier
      `Prelude.hashWithSalt` hsmConfigurationIdentifier
      `Prelude.hashWithSalt` status

instance Prelude.NFData HsmStatus where
  rnf HsmStatus' {..} =
    Prelude.rnf hsmClientCertificateIdentifier `Prelude.seq`
      Prelude.rnf hsmConfigurationIdentifier `Prelude.seq`
        Prelude.rnf status
