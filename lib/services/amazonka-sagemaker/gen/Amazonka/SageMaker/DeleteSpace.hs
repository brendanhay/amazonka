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
-- Module      : Amazonka.SageMaker.DeleteSpace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to delete a space.
module Amazonka.SageMaker.DeleteSpace
  ( -- * Creating a Request
    DeleteSpace (..),
    newDeleteSpace,

    -- * Request Lenses
    deleteSpace_domainId,
    deleteSpace_spaceName,

    -- * Destructuring the Response
    DeleteSpaceResponse (..),
    newDeleteSpaceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteSpace' smart constructor.
data DeleteSpace = DeleteSpace'
  { -- | The ID of the associated Domain.
    domainId :: Prelude.Text,
    -- | The name of the space.
    spaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSpace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'deleteSpace_domainId' - The ID of the associated Domain.
--
-- 'spaceName', 'deleteSpace_spaceName' - The name of the space.
newDeleteSpace ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'spaceName'
  Prelude.Text ->
  DeleteSpace
newDeleteSpace pDomainId_ pSpaceName_ =
  DeleteSpace'
    { domainId = pDomainId_,
      spaceName = pSpaceName_
    }

-- | The ID of the associated Domain.
deleteSpace_domainId :: Lens.Lens' DeleteSpace Prelude.Text
deleteSpace_domainId = Lens.lens (\DeleteSpace' {domainId} -> domainId) (\s@DeleteSpace' {} a -> s {domainId = a} :: DeleteSpace)

-- | The name of the space.
deleteSpace_spaceName :: Lens.Lens' DeleteSpace Prelude.Text
deleteSpace_spaceName = Lens.lens (\DeleteSpace' {spaceName} -> spaceName) (\s@DeleteSpace' {} a -> s {spaceName = a} :: DeleteSpace)

instance Core.AWSRequest DeleteSpace where
  type AWSResponse DeleteSpace = DeleteSpaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteSpaceResponse'

instance Prelude.Hashable DeleteSpace where
  hashWithSalt _salt DeleteSpace' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` spaceName

instance Prelude.NFData DeleteSpace where
  rnf DeleteSpace' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf spaceName

instance Data.ToHeaders DeleteSpace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteSpace" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSpace where
  toJSON DeleteSpace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("SpaceName" Data..= spaceName)
          ]
      )

instance Data.ToPath DeleteSpace where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSpace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSpaceResponse' smart constructor.
data DeleteSpaceResponse = DeleteSpaceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSpaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSpaceResponse ::
  DeleteSpaceResponse
newDeleteSpaceResponse = DeleteSpaceResponse'

instance Prelude.NFData DeleteSpaceResponse where
  rnf _ = ()
