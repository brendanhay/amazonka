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
-- Module      : Amazonka.Redshift.Types.ElasticIpStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ElasticIpStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes the status of the elastic IP (EIP) address.
--
-- /See:/ 'newElasticIpStatus' smart constructor.
data ElasticIpStatus = ElasticIpStatus'
  { -- | The elastic IP (EIP) address for the cluster.
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | The status of the elastic IP (EIP) address.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticIpStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticIp', 'elasticIpStatus_elasticIp' - The elastic IP (EIP) address for the cluster.
--
-- 'status', 'elasticIpStatus_status' - The status of the elastic IP (EIP) address.
newElasticIpStatus ::
  ElasticIpStatus
newElasticIpStatus =
  ElasticIpStatus'
    { elasticIp = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The elastic IP (EIP) address for the cluster.
elasticIpStatus_elasticIp :: Lens.Lens' ElasticIpStatus (Prelude.Maybe Prelude.Text)
elasticIpStatus_elasticIp = Lens.lens (\ElasticIpStatus' {elasticIp} -> elasticIp) (\s@ElasticIpStatus' {} a -> s {elasticIp = a} :: ElasticIpStatus)

-- | The status of the elastic IP (EIP) address.
elasticIpStatus_status :: Lens.Lens' ElasticIpStatus (Prelude.Maybe Prelude.Text)
elasticIpStatus_status = Lens.lens (\ElasticIpStatus' {status} -> status) (\s@ElasticIpStatus' {} a -> s {status = a} :: ElasticIpStatus)

instance Core.FromXML ElasticIpStatus where
  parseXML x =
    ElasticIpStatus'
      Prelude.<$> (x Core..@? "ElasticIp")
      Prelude.<*> (x Core..@? "Status")

instance Prelude.Hashable ElasticIpStatus where
  hashWithSalt _salt ElasticIpStatus' {..} =
    _salt `Prelude.hashWithSalt` elasticIp
      `Prelude.hashWithSalt` status

instance Prelude.NFData ElasticIpStatus where
  rnf ElasticIpStatus' {..} =
    Prelude.rnf elasticIp
      `Prelude.seq` Prelude.rnf status
