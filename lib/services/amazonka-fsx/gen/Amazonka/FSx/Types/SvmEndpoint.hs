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
-- Module      : Amazonka.FSx.Types.SvmEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SvmEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon FSx for NetApp ONTAP storage virtual machine (SVM) has four
-- endpoints that are used to access data or to manage the SVM using the
-- NetApp ONTAP CLI, REST API, or NetApp CloudManager. They are the
-- @Iscsi@, @Management@, @Nfs@, and @Smb@ endpoints.
--
-- /See:/ 'newSvmEndpoint' smart constructor.
data SvmEndpoint = SvmEndpoint'
  { dNSName :: Prelude.Maybe Prelude.Text,
    -- | The SVM endpoint\'s IP addresses.
    ipAddresses :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SvmEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dNSName', 'svmEndpoint_dNSName' - Undocumented member.
--
-- 'ipAddresses', 'svmEndpoint_ipAddresses' - The SVM endpoint\'s IP addresses.
newSvmEndpoint ::
  SvmEndpoint
newSvmEndpoint =
  SvmEndpoint'
    { dNSName = Prelude.Nothing,
      ipAddresses = Prelude.Nothing
    }

-- | Undocumented member.
svmEndpoint_dNSName :: Lens.Lens' SvmEndpoint (Prelude.Maybe Prelude.Text)
svmEndpoint_dNSName = Lens.lens (\SvmEndpoint' {dNSName} -> dNSName) (\s@SvmEndpoint' {} a -> s {dNSName = a} :: SvmEndpoint)

-- | The SVM endpoint\'s IP addresses.
svmEndpoint_ipAddresses :: Lens.Lens' SvmEndpoint (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
svmEndpoint_ipAddresses = Lens.lens (\SvmEndpoint' {ipAddresses} -> ipAddresses) (\s@SvmEndpoint' {} a -> s {ipAddresses = a} :: SvmEndpoint) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SvmEndpoint where
  parseJSON =
    Data.withObject
      "SvmEndpoint"
      ( \x ->
          SvmEndpoint'
            Prelude.<$> (x Data..:? "DNSName")
            Prelude.<*> (x Data..:? "IpAddresses")
      )

instance Prelude.Hashable SvmEndpoint where
  hashWithSalt _salt SvmEndpoint' {..} =
    _salt `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` ipAddresses

instance Prelude.NFData SvmEndpoint where
  rnf SvmEndpoint' {..} =
    Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf ipAddresses
