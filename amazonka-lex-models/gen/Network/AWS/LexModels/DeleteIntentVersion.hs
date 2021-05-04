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

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteIntentVersion' smart constructor.
data DeleteIntentVersion = DeleteIntentVersion'
  { -- | The name of the intent.
    name :: Prelude.Text,
    -- | The version of the intent to delete. You cannot delete the @$LATEST@
    -- version of the intent. To delete the @$LATEST@ version, use the
    -- DeleteIntent operation.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteIntentVersion where
  type
    Rs DeleteIntentVersion =
      DeleteIntentVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteIntentVersionResponse'

instance Prelude.Hashable DeleteIntentVersion

instance Prelude.NFData DeleteIntentVersion

instance Prelude.ToHeaders DeleteIntentVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteIntentVersion where
  toPath DeleteIntentVersion' {..} =
    Prelude.mconcat
      [ "/intents/",
        Prelude.toBS name,
        "/versions/",
        Prelude.toBS version
      ]

instance Prelude.ToQuery DeleteIntentVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntentVersionResponse' smart constructor.
data DeleteIntentVersionResponse = DeleteIntentVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntentVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntentVersionResponse ::
  DeleteIntentVersionResponse
newDeleteIntentVersionResponse =
  DeleteIntentVersionResponse'

instance Prelude.NFData DeleteIntentVersionResponse
