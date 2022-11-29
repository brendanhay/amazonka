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
-- Module      : Amazonka.Redshift.Types.EndpointAuthorization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.EndpointAuthorization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.AuthorizationStatus

-- | Describes an endpoint authorization for authorizing Redshift-managed VPC
-- endpoint access to a cluster across Amazon Web Services accounts.
--
-- /See:/ 'newEndpointAuthorization' smart constructor.
data EndpointAuthorization = EndpointAuthorization'
  { -- | The cluster identifier.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The number of Redshift-managed VPC endpoints created for the
    -- authorization.
    endpointCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether all VPCs in the grantee account are allowed access to
    -- the cluster.
    allowedAllVPCs :: Prelude.Maybe Prelude.Bool,
    -- | The status of the authorization action.
    status :: Prelude.Maybe AuthorizationStatus,
    -- | The time (UTC) when the authorization was created.
    authorizeTime :: Prelude.Maybe Core.ISO8601,
    -- | The status of the cluster.
    clusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the cluster owner.
    grantor :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the grantee of the cluster.
    grantee :: Prelude.Maybe Prelude.Text,
    -- | The VPCs allowed access to the cluster.
    allowedVPCs :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'endpointAuthorization_clusterIdentifier' - The cluster identifier.
--
-- 'endpointCount', 'endpointAuthorization_endpointCount' - The number of Redshift-managed VPC endpoints created for the
-- authorization.
--
-- 'allowedAllVPCs', 'endpointAuthorization_allowedAllVPCs' - Indicates whether all VPCs in the grantee account are allowed access to
-- the cluster.
--
-- 'status', 'endpointAuthorization_status' - The status of the authorization action.
--
-- 'authorizeTime', 'endpointAuthorization_authorizeTime' - The time (UTC) when the authorization was created.
--
-- 'clusterStatus', 'endpointAuthorization_clusterStatus' - The status of the cluster.
--
-- 'grantor', 'endpointAuthorization_grantor' - The Amazon Web Services account ID of the cluster owner.
--
-- 'grantee', 'endpointAuthorization_grantee' - The Amazon Web Services account ID of the grantee of the cluster.
--
-- 'allowedVPCs', 'endpointAuthorization_allowedVPCs' - The VPCs allowed access to the cluster.
newEndpointAuthorization ::
  EndpointAuthorization
newEndpointAuthorization =
  EndpointAuthorization'
    { clusterIdentifier =
        Prelude.Nothing,
      endpointCount = Prelude.Nothing,
      allowedAllVPCs = Prelude.Nothing,
      status = Prelude.Nothing,
      authorizeTime = Prelude.Nothing,
      clusterStatus = Prelude.Nothing,
      grantor = Prelude.Nothing,
      grantee = Prelude.Nothing,
      allowedVPCs = Prelude.Nothing
    }

-- | The cluster identifier.
endpointAuthorization_clusterIdentifier :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_clusterIdentifier = Lens.lens (\EndpointAuthorization' {clusterIdentifier} -> clusterIdentifier) (\s@EndpointAuthorization' {} a -> s {clusterIdentifier = a} :: EndpointAuthorization)

-- | The number of Redshift-managed VPC endpoints created for the
-- authorization.
endpointAuthorization_endpointCount :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Int)
endpointAuthorization_endpointCount = Lens.lens (\EndpointAuthorization' {endpointCount} -> endpointCount) (\s@EndpointAuthorization' {} a -> s {endpointCount = a} :: EndpointAuthorization)

-- | Indicates whether all VPCs in the grantee account are allowed access to
-- the cluster.
endpointAuthorization_allowedAllVPCs :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Bool)
endpointAuthorization_allowedAllVPCs = Lens.lens (\EndpointAuthorization' {allowedAllVPCs} -> allowedAllVPCs) (\s@EndpointAuthorization' {} a -> s {allowedAllVPCs = a} :: EndpointAuthorization)

-- | The status of the authorization action.
endpointAuthorization_status :: Lens.Lens' EndpointAuthorization (Prelude.Maybe AuthorizationStatus)
endpointAuthorization_status = Lens.lens (\EndpointAuthorization' {status} -> status) (\s@EndpointAuthorization' {} a -> s {status = a} :: EndpointAuthorization)

-- | The time (UTC) when the authorization was created.
endpointAuthorization_authorizeTime :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.UTCTime)
endpointAuthorization_authorizeTime = Lens.lens (\EndpointAuthorization' {authorizeTime} -> authorizeTime) (\s@EndpointAuthorization' {} a -> s {authorizeTime = a} :: EndpointAuthorization) Prelude.. Lens.mapping Core._Time

-- | The status of the cluster.
endpointAuthorization_clusterStatus :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_clusterStatus = Lens.lens (\EndpointAuthorization' {clusterStatus} -> clusterStatus) (\s@EndpointAuthorization' {} a -> s {clusterStatus = a} :: EndpointAuthorization)

-- | The Amazon Web Services account ID of the cluster owner.
endpointAuthorization_grantor :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_grantor = Lens.lens (\EndpointAuthorization' {grantor} -> grantor) (\s@EndpointAuthorization' {} a -> s {grantor = a} :: EndpointAuthorization)

-- | The Amazon Web Services account ID of the grantee of the cluster.
endpointAuthorization_grantee :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_grantee = Lens.lens (\EndpointAuthorization' {grantee} -> grantee) (\s@EndpointAuthorization' {} a -> s {grantee = a} :: EndpointAuthorization)

-- | The VPCs allowed access to the cluster.
endpointAuthorization_allowedVPCs :: Lens.Lens' EndpointAuthorization (Prelude.Maybe [Prelude.Text])
endpointAuthorization_allowedVPCs = Lens.lens (\EndpointAuthorization' {allowedVPCs} -> allowedVPCs) (\s@EndpointAuthorization' {} a -> s {allowedVPCs = a} :: EndpointAuthorization) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML EndpointAuthorization where
  parseXML x =
    EndpointAuthorization'
      Prelude.<$> (x Core..@? "ClusterIdentifier")
      Prelude.<*> (x Core..@? "EndpointCount")
      Prelude.<*> (x Core..@? "AllowedAllVPCs")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "AuthorizeTime")
      Prelude.<*> (x Core..@? "ClusterStatus")
      Prelude.<*> (x Core..@? "Grantor")
      Prelude.<*> (x Core..@? "Grantee")
      Prelude.<*> ( x Core..@? "AllowedVPCs" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "VpcIdentifier")
                  )

instance Prelude.Hashable EndpointAuthorization where
  hashWithSalt _salt EndpointAuthorization' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` endpointCount
      `Prelude.hashWithSalt` allowedAllVPCs
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` authorizeTime
      `Prelude.hashWithSalt` clusterStatus
      `Prelude.hashWithSalt` grantor
      `Prelude.hashWithSalt` grantee
      `Prelude.hashWithSalt` allowedVPCs

instance Prelude.NFData EndpointAuthorization where
  rnf EndpointAuthorization' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf endpointCount
      `Prelude.seq` Prelude.rnf allowedAllVPCs
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf authorizeTime
      `Prelude.seq` Prelude.rnf clusterStatus
      `Prelude.seq` Prelude.rnf grantor
      `Prelude.seq` Prelude.rnf grantee
      `Prelude.seq` Prelude.rnf allowedVPCs
