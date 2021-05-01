{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.DeleteDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to delete a domain. If you onboarded with IAM mode, you will need
-- to delete your domain to onboard again using SSO. Use with caution. All
-- of the members of the domain will lose access to their EFS volume,
-- including data, notebooks, and other artifacts.
module Network.AWS.SageMaker.DeleteDomain
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The retention policy for this domain, which specifies whether resources
    -- will be retained after the Domain is deleted. By default, all resources
    -- are retained (not automatically deleted).
    retentionPolicy :: Prelude.Maybe RetentionPolicy,
    -- | The domain ID.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteDomainResponse'

instance Prelude.Hashable DeleteDomain

instance Prelude.NFData DeleteDomain

instance Prelude.ToHeaders DeleteDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteDomain" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RetentionPolicy" Prelude..=)
              Prelude.<$> retentionPolicy,
            Prelude.Just ("DomainId" Prelude..= domainId)
          ]
      )

instance Prelude.ToPath DeleteDomain where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDomainResponse ::
  DeleteDomainResponse
newDeleteDomainResponse = DeleteDomainResponse'

instance Prelude.NFData DeleteDomainResponse
