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
-- Module      : Network.AWS.VoiceId.DeleteDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain from the Amazon Connect Voice ID system.
module Network.AWS.VoiceId.DeleteDomain
  ( -- * Creating a Request
    DeleteDomain (..),
    newDeleteDomain,

    -- * Request Lenses
    deleteDomain_domainId,

    -- * Destructuring the Response
    DeleteDomainResponse (..),
    newDeleteDomainResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.VoiceId.Types

-- | /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The identifier of the domain you want to delete.
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
-- 'domainId', 'deleteDomain_domainId' - The identifier of the domain you want to delete.
newDeleteDomain ::
  -- | 'domainId'
  Prelude.Text ->
  DeleteDomain
newDeleteDomain pDomainId_ =
  DeleteDomain' {domainId = pDomainId_}

-- | The identifier of the domain you want to delete.
deleteDomain_domainId :: Lens.Lens' DeleteDomain Prelude.Text
deleteDomain_domainId = Lens.lens (\DeleteDomain' {domainId} -> domainId) (\s@DeleteDomain' {} a -> s {domainId = a} :: DeleteDomain)

instance Core.AWSRequest DeleteDomain where
  type AWSResponse DeleteDomain = DeleteDomainResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteDomainResponse'

instance Prelude.Hashable DeleteDomain

instance Prelude.NFData DeleteDomain

instance Core.ToHeaders DeleteDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("VoiceID.DeleteDomain" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainId" Core..= domainId)]
      )

instance Core.ToPath DeleteDomain where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDomain where
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

instance Prelude.NFData DeleteDomainResponse
