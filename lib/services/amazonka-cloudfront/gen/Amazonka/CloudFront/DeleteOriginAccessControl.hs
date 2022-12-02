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
-- Module      : Amazonka.CloudFront.DeleteOriginAccessControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CloudFront origin access control.
--
-- You cannot delete an origin access control if it\'s in use. First,
-- update all distributions to remove the origin access control from all
-- origins, then delete the origin access control.
module Amazonka.CloudFront.DeleteOriginAccessControl
  ( -- * Creating a Request
    DeleteOriginAccessControl (..),
    newDeleteOriginAccessControl,

    -- * Request Lenses
    deleteOriginAccessControl_ifMatch,
    deleteOriginAccessControl_id,

    -- * Destructuring the Response
    DeleteOriginAccessControlResponse (..),
    newDeleteOriginAccessControlResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOriginAccessControl' smart constructor.
data DeleteOriginAccessControl = DeleteOriginAccessControl'
  { -- | The current version (@ETag@ value) of the origin access control that you
    -- are deleting.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the origin access control that you are
    -- deleting.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginAccessControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteOriginAccessControl_ifMatch' - The current version (@ETag@ value) of the origin access control that you
-- are deleting.
--
-- 'id', 'deleteOriginAccessControl_id' - The unique identifier of the origin access control that you are
-- deleting.
newDeleteOriginAccessControl ::
  -- | 'id'
  Prelude.Text ->
  DeleteOriginAccessControl
newDeleteOriginAccessControl pId_ =
  DeleteOriginAccessControl'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The current version (@ETag@ value) of the origin access control that you
-- are deleting.
deleteOriginAccessControl_ifMatch :: Lens.Lens' DeleteOriginAccessControl (Prelude.Maybe Prelude.Text)
deleteOriginAccessControl_ifMatch = Lens.lens (\DeleteOriginAccessControl' {ifMatch} -> ifMatch) (\s@DeleteOriginAccessControl' {} a -> s {ifMatch = a} :: DeleteOriginAccessControl)

-- | The unique identifier of the origin access control that you are
-- deleting.
deleteOriginAccessControl_id :: Lens.Lens' DeleteOriginAccessControl Prelude.Text
deleteOriginAccessControl_id = Lens.lens (\DeleteOriginAccessControl' {id} -> id) (\s@DeleteOriginAccessControl' {} a -> s {id = a} :: DeleteOriginAccessControl)

instance Core.AWSRequest DeleteOriginAccessControl where
  type
    AWSResponse DeleteOriginAccessControl =
      DeleteOriginAccessControlResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteOriginAccessControlResponse'

instance Prelude.Hashable DeleteOriginAccessControl where
  hashWithSalt _salt DeleteOriginAccessControl' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteOriginAccessControl where
  rnf DeleteOriginAccessControl' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteOriginAccessControl where
  toHeaders DeleteOriginAccessControl' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath DeleteOriginAccessControl where
  toPath DeleteOriginAccessControl' {..} =
    Prelude.mconcat
      ["/2020-05-31/origin-access-control/", Data.toBS id]

instance Data.ToQuery DeleteOriginAccessControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOriginAccessControlResponse' smart constructor.
data DeleteOriginAccessControlResponse = DeleteOriginAccessControlResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginAccessControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOriginAccessControlResponse ::
  DeleteOriginAccessControlResponse
newDeleteOriginAccessControlResponse =
  DeleteOriginAccessControlResponse'

instance
  Prelude.NFData
    DeleteOriginAccessControlResponse
  where
  rnf _ = ()
