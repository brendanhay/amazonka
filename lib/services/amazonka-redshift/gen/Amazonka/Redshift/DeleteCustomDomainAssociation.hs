{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.DeleteCustomDomainAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Contains information about deleting a custom domain association for a
-- cluster.
module Amazonka.Redshift.DeleteCustomDomainAssociation
  ( -- * Creating a Request
    DeleteCustomDomainAssociation (..),
    newDeleteCustomDomainAssociation,

    -- * Request Lenses
    deleteCustomDomainAssociation_clusterIdentifier,

    -- * Destructuring the Response
    DeleteCustomDomainAssociationResponse (..),
    newDeleteCustomDomainAssociationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomDomainAssociation' smart constructor.
data DeleteCustomDomainAssociation = DeleteCustomDomainAssociation'
  { -- | The identifier of the cluster to delete a custom domain association for.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomDomainAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'deleteCustomDomainAssociation_clusterIdentifier' - The identifier of the cluster to delete a custom domain association for.
newDeleteCustomDomainAssociation ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  DeleteCustomDomainAssociation
newDeleteCustomDomainAssociation pClusterIdentifier_ =
  DeleteCustomDomainAssociation'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster to delete a custom domain association for.
deleteCustomDomainAssociation_clusterIdentifier :: Lens.Lens' DeleteCustomDomainAssociation Prelude.Text
deleteCustomDomainAssociation_clusterIdentifier = Lens.lens (\DeleteCustomDomainAssociation' {clusterIdentifier} -> clusterIdentifier) (\s@DeleteCustomDomainAssociation' {} a -> s {clusterIdentifier = a} :: DeleteCustomDomainAssociation)

instance
  Core.AWSRequest
    DeleteCustomDomainAssociation
  where
  type
    AWSResponse DeleteCustomDomainAssociation =
      DeleteCustomDomainAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteCustomDomainAssociationResponse'

instance
  Prelude.Hashable
    DeleteCustomDomainAssociation
  where
  hashWithSalt _salt DeleteCustomDomainAssociation' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData DeleteCustomDomainAssociation where
  rnf DeleteCustomDomainAssociation' {..} =
    Prelude.rnf clusterIdentifier

instance Data.ToHeaders DeleteCustomDomainAssociation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCustomDomainAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCustomDomainAssociation where
  toQuery DeleteCustomDomainAssociation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteCustomDomainAssociation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newDeleteCustomDomainAssociationResponse' smart constructor.
data DeleteCustomDomainAssociationResponse = DeleteCustomDomainAssociationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomDomainAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomDomainAssociationResponse ::
  DeleteCustomDomainAssociationResponse
newDeleteCustomDomainAssociationResponse =
  DeleteCustomDomainAssociationResponse'

instance
  Prelude.NFData
    DeleteCustomDomainAssociationResponse
  where
  rnf _ = ()
