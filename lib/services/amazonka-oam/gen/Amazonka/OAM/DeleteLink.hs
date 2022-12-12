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
-- Module      : Amazonka.OAM.DeleteLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a link between a monitoring account sink and a source account.
-- You must run this operation in the source account.
module Amazonka.OAM.DeleteLink
  ( -- * Creating a Request
    DeleteLink (..),
    newDeleteLink,

    -- * Request Lenses
    deleteLink_identifier,

    -- * Destructuring the Response
    DeleteLinkResponse (..),
    newDeleteLinkResponse,

    -- * Response Lenses
    deleteLinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLink' smart constructor.
data DeleteLink = DeleteLink'
  { -- | The ARN of the link to delete.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'deleteLink_identifier' - The ARN of the link to delete.
newDeleteLink ::
  -- | 'identifier'
  Prelude.Text ->
  DeleteLink
newDeleteLink pIdentifier_ =
  DeleteLink' {identifier = pIdentifier_}

-- | The ARN of the link to delete.
deleteLink_identifier :: Lens.Lens' DeleteLink Prelude.Text
deleteLink_identifier = Lens.lens (\DeleteLink' {identifier} -> identifier) (\s@DeleteLink' {} a -> s {identifier = a} :: DeleteLink)

instance Core.AWSRequest DeleteLink where
  type AWSResponse DeleteLink = DeleteLinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLinkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLink where
  hashWithSalt _salt DeleteLink' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteLink where
  rnf DeleteLink' {..} = Prelude.rnf identifier

instance Data.ToHeaders DeleteLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLink where
  toJSON DeleteLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )

instance Data.ToPath DeleteLink where
  toPath = Prelude.const "/DeleteLink"

instance Data.ToQuery DeleteLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLinkResponse' smart constructor.
data DeleteLinkResponse = DeleteLinkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLinkResponse_httpStatus' - The response's http status code.
newDeleteLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLinkResponse
newDeleteLinkResponse pHttpStatus_ =
  DeleteLinkResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLinkResponse_httpStatus :: Lens.Lens' DeleteLinkResponse Prelude.Int
deleteLinkResponse_httpStatus = Lens.lens (\DeleteLinkResponse' {httpStatus} -> httpStatus) (\s@DeleteLinkResponse' {} a -> s {httpStatus = a} :: DeleteLinkResponse)

instance Prelude.NFData DeleteLinkResponse where
  rnf DeleteLinkResponse' {..} = Prelude.rnf httpStatus
