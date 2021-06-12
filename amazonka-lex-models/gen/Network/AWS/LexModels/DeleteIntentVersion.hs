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
-- Module      : Network.AWS.LexModels.DeleteIntentVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of an intent. To delete all versions of a
-- intent, use the DeleteIntent operation.
--
-- This operation requires permissions for the @lex:DeleteIntentVersion@
-- action.
module Network.AWS.LexModels.DeleteIntentVersion
  ( -- * Creating a Request
    DeleteIntentVersion (..),
    newDeleteIntentVersion,

    -- * Request Lenses
    deleteIntentVersion_name,
    deleteIntentVersion_version,

    -- * Destructuring the Response
    DeleteIntentVersionResponse (..),
    newDeleteIntentVersionResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteIntentVersion' smart constructor.
data DeleteIntentVersion = DeleteIntentVersion'
  { -- | The name of the intent.
    name :: Core.Text,
    -- | The version of the intent to delete. You cannot delete the @$LATEST@
    -- version of the intent. To delete the @$LATEST@ version, use the
    -- DeleteIntent operation.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIntentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteIntentVersion_name' - The name of the intent.
--
-- 'version', 'deleteIntentVersion_version' - The version of the intent to delete. You cannot delete the @$LATEST@
-- version of the intent. To delete the @$LATEST@ version, use the
-- DeleteIntent operation.
newDeleteIntentVersion ::
  -- | 'name'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  DeleteIntentVersion
newDeleteIntentVersion pName_ pVersion_ =
  DeleteIntentVersion'
    { name = pName_,
      version = pVersion_
    }

-- | The name of the intent.
deleteIntentVersion_name :: Lens.Lens' DeleteIntentVersion Core.Text
deleteIntentVersion_name = Lens.lens (\DeleteIntentVersion' {name} -> name) (\s@DeleteIntentVersion' {} a -> s {name = a} :: DeleteIntentVersion)

-- | The version of the intent to delete. You cannot delete the @$LATEST@
-- version of the intent. To delete the @$LATEST@ version, use the
-- DeleteIntent operation.
deleteIntentVersion_version :: Lens.Lens' DeleteIntentVersion Core.Text
deleteIntentVersion_version = Lens.lens (\DeleteIntentVersion' {version} -> version) (\s@DeleteIntentVersion' {} a -> s {version = a} :: DeleteIntentVersion)

instance Core.AWSRequest DeleteIntentVersion where
  type
    AWSResponse DeleteIntentVersion =
      DeleteIntentVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteIntentVersionResponse'

instance Core.Hashable DeleteIntentVersion

instance Core.NFData DeleteIntentVersion

instance Core.ToHeaders DeleteIntentVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteIntentVersion where
  toPath DeleteIntentVersion' {..} =
    Core.mconcat
      [ "/intents/",
        Core.toBS name,
        "/versions/",
        Core.toBS version
      ]

instance Core.ToQuery DeleteIntentVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteIntentVersionResponse' smart constructor.
data DeleteIntentVersionResponse = DeleteIntentVersionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIntentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntentVersionResponse ::
  DeleteIntentVersionResponse
newDeleteIntentVersionResponse =
  DeleteIntentVersionResponse'

instance Core.NFData DeleteIntentVersionResponse
