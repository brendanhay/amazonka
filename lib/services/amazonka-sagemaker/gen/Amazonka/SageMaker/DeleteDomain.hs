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
-- Module      : Amazonka.SageMaker.DeleteDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to delete a domain. If you onboarded with IAM mode, you will need
-- to delete your domain to onboard again using IAM Identity Center. Use
-- with caution. All of the members of the domain will lose access to their
-- EFS volume, including data, notebooks, and other artifacts.
module Amazonka.SageMaker.DeleteDomain
  ( -- * Creating a Request
    DeleteDomain (..),
    newDeleteDomain,

    -- * Request Lenses
    deleteDomain_retentionPolicy,
    deleteDomain_domainId,

    -- * Destructuring the Response
    DeleteDomainResponse (..),
    newDeleteDomainResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The retention policy for this domain, which specifies whether resources
    -- will be retained after the Domain is deleted. By default, all resources
    -- are retained (not automatically deleted).
    retentionPolicy :: Prelude.Maybe RetentionPolicy,
    -- | The domain ID.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPolicy', 'deleteDomain_retentionPolicy' - The retention policy for this domain, which specifies whether resources
-- will be retained after the Domain is deleted. By default, all resources
-- are retained (not automatically deleted).
--
-- 'domainId', 'deleteDomain_domainId' - The domain ID.
newDeleteDomain ::
  -- | 'domainId'
  Prelude.Text ->
  DeleteDomain
newDeleteDomain pDomainId_ =
  DeleteDomain'
    { retentionPolicy = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The retention policy for this domain, which specifies whether resources
-- will be retained after the Domain is deleted. By default, all resources
-- are retained (not automatically deleted).
deleteDomain_retentionPolicy :: Lens.Lens' DeleteDomain (Prelude.Maybe RetentionPolicy)
deleteDomain_retentionPolicy = Lens.lens (\DeleteDomain' {retentionPolicy} -> retentionPolicy) (\s@DeleteDomain' {} a -> s {retentionPolicy = a} :: DeleteDomain)

-- | The domain ID.
deleteDomain_domainId :: Lens.Lens' DeleteDomain Prelude.Text
deleteDomain_domainId = Lens.lens (\DeleteDomain' {domainId} -> domainId) (\s@DeleteDomain' {} a -> s {domainId = a} :: DeleteDomain)

instance Core.AWSRequest DeleteDomain where
  type AWSResponse DeleteDomain = DeleteDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteDomainResponse'

instance Prelude.Hashable DeleteDomain where
  hashWithSalt _salt DeleteDomain' {..} =
    _salt
      `Prelude.hashWithSalt` retentionPolicy
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData DeleteDomain where
  rnf DeleteDomain' {..} =
    Prelude.rnf retentionPolicy
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders DeleteDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteDomain" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RetentionPolicy" Data..=)
              Prelude.<$> retentionPolicy,
            Prelude.Just ("DomainId" Data..= domainId)
          ]
      )

instance Data.ToPath DeleteDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDomainResponse ::
  DeleteDomainResponse
newDeleteDomainResponse = DeleteDomainResponse'

instance Prelude.NFData DeleteDomainResponse where
  rnf _ = ()
