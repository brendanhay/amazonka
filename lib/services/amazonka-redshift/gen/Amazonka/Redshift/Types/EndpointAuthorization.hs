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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.EndpointAuthorization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.AuthorizationStatus

-- | Describes an endpoint authorization for authorizing Redshift-managed VPC
-- endpoint access to a cluster across Amazon Web Services accounts.
--
-- /See:/ 'newEndpointAuthorization' smart constructor.
data EndpointAuthorization = EndpointAuthorization'
  { -- | Indicates whether all VPCs in the grantee account are allowed access to
    -- the cluster.
    allowedAllVPCs :: Prelude.Maybe Prelude.Bool,
    -- | The VPCs allowed access to the cluster.
    allowedVPCs :: Prelude.Maybe [Prelude.Text],
    -- | The time (UTC) when the authorization was created.
    authorizeTime :: Prelude.Maybe Data.ISO8601,
    -- | The cluster identifier.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The status of the cluster.
    clusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The number of Redshift-managed VPC endpoints created for the
    -- authorization.
    endpointCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services account ID of the grantee of the cluster.
    grantee :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the cluster owner.
    grantor :: Prelude.Maybe Prelude.Text,
    -- | The status of the authorization action.
    status :: Prelude.Maybe AuthorizationStatus
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
-- 'allowedAllVPCs', 'endpointAuthorization_allowedAllVPCs' - Indicates whether all VPCs in the grantee account are allowed access to
-- the cluster.
--
-- 'allowedVPCs', 'endpointAuthorization_allowedVPCs' - The VPCs allowed access to the cluster.
--
-- 'authorizeTime', 'endpointAuthorization_authorizeTime' - The time (UTC) when the authorization was created.
--
-- 'clusterIdentifier', 'endpointAuthorization_clusterIdentifier' - The cluster identifier.
--
-- 'clusterStatus', 'endpointAuthorization_clusterStatus' - The status of the cluster.
--
-- 'endpointCount', 'endpointAuthorization_endpointCount' - The number of Redshift-managed VPC endpoints created for the
-- authorization.
--
-- 'grantee', 'endpointAuthorization_grantee' - The Amazon Web Services account ID of the grantee of the cluster.
--
-- 'grantor', 'endpointAuthorization_grantor' - The Amazon Web Services account ID of the cluster owner.
--
-- 'status', 'endpointAuthorization_status' - The status of the authorization action.
newEndpointAuthorization ::
  EndpointAuthorization
newEndpointAuthorization =
  EndpointAuthorization'
    { allowedAllVPCs =
        Prelude.Nothing,
      allowedVPCs = Prelude.Nothing,
      authorizeTime = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      clusterStatus = Prelude.Nothing,
      endpointCount = Prelude.Nothing,
      grantee = Prelude.Nothing,
      grantor = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Indicates whether all VPCs in the grantee account are allowed access to
-- the cluster.
endpointAuthorization_allowedAllVPCs :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Bool)
endpointAuthorization_allowedAllVPCs = Lens.lens (\EndpointAuthorization' {allowedAllVPCs} -> allowedAllVPCs) (\s@EndpointAuthorization' {} a -> s {allowedAllVPCs = a} :: EndpointAuthorization)

-- | The VPCs allowed access to the cluster.
endpointAuthorization_allowedVPCs :: Lens.Lens' EndpointAuthorization (Prelude.Maybe [Prelude.Text])
endpointAuthorization_allowedVPCs = Lens.lens (\EndpointAuthorization' {allowedVPCs} -> allowedVPCs) (\s@EndpointAuthorization' {} a -> s {allowedVPCs = a} :: EndpointAuthorization) Prelude.. Lens.mapping Lens.coerced

-- | The time (UTC) when the authorization was created.
endpointAuthorization_authorizeTime :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.UTCTime)
endpointAuthorization_authorizeTime = Lens.lens (\EndpointAuthorization' {authorizeTime} -> authorizeTime) (\s@EndpointAuthorization' {} a -> s {authorizeTime = a} :: EndpointAuthorization) Prelude.. Lens.mapping Data._Time

-- | The cluster identifier.
endpointAuthorization_clusterIdentifier :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_clusterIdentifier = Lens.lens (\EndpointAuthorization' {clusterIdentifier} -> clusterIdentifier) (\s@EndpointAuthorization' {} a -> s {clusterIdentifier = a} :: EndpointAuthorization)

-- | The status of the cluster.
endpointAuthorization_clusterStatus :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_clusterStatus = Lens.lens (\EndpointAuthorization' {clusterStatus} -> clusterStatus) (\s@EndpointAuthorization' {} a -> s {clusterStatus = a} :: EndpointAuthorization)

-- | The number of Redshift-managed VPC endpoints created for the
-- authorization.
endpointAuthorization_endpointCount :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Int)
endpointAuthorization_endpointCount = Lens.lens (\EndpointAuthorization' {endpointCount} -> endpointCount) (\s@EndpointAuthorization' {} a -> s {endpointCount = a} :: EndpointAuthorization)

-- | The Amazon Web Services account ID of the grantee of the cluster.
endpointAuthorization_grantee :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_grantee = Lens.lens (\EndpointAuthorization' {grantee} -> grantee) (\s@EndpointAuthorization' {} a -> s {grantee = a} :: EndpointAuthorization)

-- | The Amazon Web Services account ID of the cluster owner.
endpointAuthorization_grantor :: Lens.Lens' EndpointAuthorization (Prelude.Maybe Prelude.Text)
endpointAuthorization_grantor = Lens.lens (\EndpointAuthorization' {grantor} -> grantor) (\s@EndpointAuthorization' {} a -> s {grantor = a} :: EndpointAuthorization)

-- | The status of the authorization action.
endpointAuthorization_status :: Lens.Lens' EndpointAuthorization (Prelude.Maybe AuthorizationStatus)
endpointAuthorization_status = Lens.lens (\EndpointAuthorization' {status} -> status) (\s@EndpointAuthorization' {} a -> s {status = a} :: EndpointAuthorization)

instance Data.FromXML EndpointAuthorization where
  parseXML x =
    EndpointAuthorization'
      Prelude.<$> (x Data..@? "AllowedAllVPCs")
      Prelude.<*> ( x Data..@? "AllowedVPCs" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "VpcIdentifier")
                  )
      Prelude.<*> (x Data..@? "AuthorizeTime")
      Prelude.<*> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "ClusterStatus")
      Prelude.<*> (x Data..@? "EndpointCount")
      Prelude.<*> (x Data..@? "Grantee")
      Prelude.<*> (x Data..@? "Grantor")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable EndpointAuthorization where
  hashWithSalt _salt EndpointAuthorization' {..} =
    _salt
      `Prelude.hashWithSalt` allowedAllVPCs
      `Prelude.hashWithSalt` allowedVPCs
      `Prelude.hashWithSalt` authorizeTime
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` clusterStatus
      `Prelude.hashWithSalt` endpointCount
      `Prelude.hashWithSalt` grantee
      `Prelude.hashWithSalt` grantor
      `Prelude.hashWithSalt` status

instance Prelude.NFData EndpointAuthorization where
  rnf EndpointAuthorization' {..} =
    Prelude.rnf allowedAllVPCs `Prelude.seq`
      Prelude.rnf allowedVPCs `Prelude.seq`
        Prelude.rnf authorizeTime `Prelude.seq`
          Prelude.rnf clusterIdentifier `Prelude.seq`
            Prelude.rnf clusterStatus `Prelude.seq`
              Prelude.rnf endpointCount `Prelude.seq`
                Prelude.rnf grantee `Prelude.seq`
                  Prelude.rnf grantor `Prelude.seq`
                    Prelude.rnf status
