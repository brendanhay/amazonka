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
-- Module      : Amazonka.EC2.Types.PrivateDnsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrivateDnsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the Private DNS name for interface endpoints.
--
-- /See:/ 'newPrivateDnsDetails' smart constructor.
data PrivateDnsDetails = PrivateDnsDetails'
  { -- | The private DNS name assigned to the VPC endpoint service.
    privateDnsName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'privateDnsName', 'privateDnsDetails_privateDnsName' - The private DNS name assigned to the VPC endpoint service.
newPrivateDnsDetails ::
  PrivateDnsDetails
newPrivateDnsDetails =
  PrivateDnsDetails'
    { privateDnsName =
        Prelude.Nothing
    }

-- | The private DNS name assigned to the VPC endpoint service.
privateDnsDetails_privateDnsName :: Lens.Lens' PrivateDnsDetails (Prelude.Maybe Prelude.Text)
privateDnsDetails_privateDnsName = Lens.lens (\PrivateDnsDetails' {privateDnsName} -> privateDnsName) (\s@PrivateDnsDetails' {} a -> s {privateDnsName = a} :: PrivateDnsDetails)

instance Data.FromXML PrivateDnsDetails where
  parseXML x =
    PrivateDnsDetails'
      Prelude.<$> (x Data..@? "privateDnsName")

instance Prelude.Hashable PrivateDnsDetails where
  hashWithSalt _salt PrivateDnsDetails' {..} =
    _salt `Prelude.hashWithSalt` privateDnsName

instance Prelude.NFData PrivateDnsDetails where
  rnf PrivateDnsDetails' {..} =
    Prelude.rnf privateDnsName
