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
-- Module      : Amazonka.CloudFront.DeleteKeyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a key group.
--
-- You cannot delete a key group that is referenced in a cache behavior.
-- First update your distributions to remove the key group from all cache
-- behaviors, then delete the key group.
--
-- To delete a key group, you must provide the key group\'s identifier and
-- version. To get these values, use @ListKeyGroups@ followed by
-- @GetKeyGroup@ or @GetKeyGroupConfig@.
module Amazonka.CloudFront.DeleteKeyGroup
  ( -- * Creating a Request
    DeleteKeyGroup (..),
    newDeleteKeyGroup,

    -- * Request Lenses
    deleteKeyGroup_ifMatch,
    deleteKeyGroup_id,

    -- * Destructuring the Response
    DeleteKeyGroupResponse (..),
    newDeleteKeyGroupResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKeyGroup' smart constructor.
data DeleteKeyGroup = DeleteKeyGroup'
  { -- | The version of the key group that you are deleting. The version is the
    -- key group\'s @ETag@ value. To get the @ETag@, use @GetKeyGroup@ or
    -- @GetKeyGroupConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the key group that you are deleting. To get the
    -- identifier, use @ListKeyGroups@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteKeyGroup_ifMatch' - The version of the key group that you are deleting. The version is the
-- key group\'s @ETag@ value. To get the @ETag@, use @GetKeyGroup@ or
-- @GetKeyGroupConfig@.
--
-- 'id', 'deleteKeyGroup_id' - The identifier of the key group that you are deleting. To get the
-- identifier, use @ListKeyGroups@.
newDeleteKeyGroup ::
  -- | 'id'
  Prelude.Text ->
  DeleteKeyGroup
newDeleteKeyGroup pId_ =
  DeleteKeyGroup'
    { ifMatch = Prelude.Nothing,
      id = pId_
    }

-- | The version of the key group that you are deleting. The version is the
-- key group\'s @ETag@ value. To get the @ETag@, use @GetKeyGroup@ or
-- @GetKeyGroupConfig@.
deleteKeyGroup_ifMatch :: Lens.Lens' DeleteKeyGroup (Prelude.Maybe Prelude.Text)
deleteKeyGroup_ifMatch = Lens.lens (\DeleteKeyGroup' {ifMatch} -> ifMatch) (\s@DeleteKeyGroup' {} a -> s {ifMatch = a} :: DeleteKeyGroup)

-- | The identifier of the key group that you are deleting. To get the
-- identifier, use @ListKeyGroups@.
deleteKeyGroup_id :: Lens.Lens' DeleteKeyGroup Prelude.Text
deleteKeyGroup_id = Lens.lens (\DeleteKeyGroup' {id} -> id) (\s@DeleteKeyGroup' {} a -> s {id = a} :: DeleteKeyGroup)

instance Core.AWSRequest DeleteKeyGroup where
  type
    AWSResponse DeleteKeyGroup =
      DeleteKeyGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteKeyGroupResponse'

instance Prelude.Hashable DeleteKeyGroup where
  hashWithSalt _salt DeleteKeyGroup' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteKeyGroup where
  rnf DeleteKeyGroup' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteKeyGroup where
  toHeaders DeleteKeyGroup' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath DeleteKeyGroup where
  toPath DeleteKeyGroup' {..} =
    Prelude.mconcat
      ["/2020-05-31/key-group/", Data.toBS id]

instance Data.ToQuery DeleteKeyGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKeyGroupResponse' smart constructor.
data DeleteKeyGroupResponse = DeleteKeyGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteKeyGroupResponse ::
  DeleteKeyGroupResponse
newDeleteKeyGroupResponse = DeleteKeyGroupResponse'

instance Prelude.NFData DeleteKeyGroupResponse where
  rnf _ = ()
