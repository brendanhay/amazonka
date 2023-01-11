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
-- Module      : Amazonka.LexModels.DeleteIntentVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.LexModels.DeleteIntentVersion
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIntentVersion' smart constructor.
data DeleteIntentVersion = DeleteIntentVersion'
  { -- | The name of the intent.
    name :: Prelude.Text,
    -- | The version of the intent to delete. You cannot delete the @$LATEST@
    -- version of the intent. To delete the @$LATEST@ version, use the
    -- DeleteIntent operation.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  DeleteIntentVersion
newDeleteIntentVersion pName_ pVersion_ =
  DeleteIntentVersion'
    { name = pName_,
      version = pVersion_
    }

-- | The name of the intent.
deleteIntentVersion_name :: Lens.Lens' DeleteIntentVersion Prelude.Text
deleteIntentVersion_name = Lens.lens (\DeleteIntentVersion' {name} -> name) (\s@DeleteIntentVersion' {} a -> s {name = a} :: DeleteIntentVersion)

-- | The version of the intent to delete. You cannot delete the @$LATEST@
-- version of the intent. To delete the @$LATEST@ version, use the
-- DeleteIntent operation.
deleteIntentVersion_version :: Lens.Lens' DeleteIntentVersion Prelude.Text
deleteIntentVersion_version = Lens.lens (\DeleteIntentVersion' {version} -> version) (\s@DeleteIntentVersion' {} a -> s {version = a} :: DeleteIntentVersion)

instance Core.AWSRequest DeleteIntentVersion where
  type
    AWSResponse DeleteIntentVersion =
      DeleteIntentVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteIntentVersionResponse'

instance Prelude.Hashable DeleteIntentVersion where
  hashWithSalt _salt DeleteIntentVersion' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData DeleteIntentVersion where
  rnf DeleteIntentVersion' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Data.ToHeaders DeleteIntentVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteIntentVersion where
  toPath DeleteIntentVersion' {..} =
    Prelude.mconcat
      [ "/intents/",
        Data.toBS name,
        "/versions/",
        Data.toBS version
      ]

instance Data.ToQuery DeleteIntentVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntentVersionResponse' smart constructor.
data DeleteIntentVersionResponse = DeleteIntentVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntentVersionResponse ::
  DeleteIntentVersionResponse
newDeleteIntentVersionResponse =
  DeleteIntentVersionResponse'

instance Prelude.NFData DeleteIntentVersionResponse where
  rnf _ = ()
