{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Redshift.Types.ElasticIpStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ElasticIpStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes the status of the elastic IP (EIP) address.
--
-- /See:/ 'newElasticIpStatus' smart constructor.
data ElasticIpStatus = ElasticIpStatus'
  { -- | The elastic IP (EIP) address for the cluster.
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | The status of the elastic IP (EIP) address.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromXML ElasticIpStatus where
  parseXML x =
    ElasticIpStatus'
      Prelude.<$> (x Prelude..@? "ElasticIp")
      Prelude.<*> (x Prelude..@? "Status")

instance Prelude.Hashable ElasticIpStatus

instance Prelude.NFData ElasticIpStatus
