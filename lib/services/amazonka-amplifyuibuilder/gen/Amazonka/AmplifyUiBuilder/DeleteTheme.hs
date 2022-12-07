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
-- Module      : Amazonka.AmplifyUiBuilder.DeleteTheme
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a theme from an Amplify app.
module Amazonka.AmplifyUiBuilder.DeleteTheme
  ( -- * Creating a Request
    DeleteTheme (..),
    newDeleteTheme,

    -- * Request Lenses
    deleteTheme_appId,
    deleteTheme_environmentName,
    deleteTheme_id,

    -- * Destructuring the Response
    DeleteThemeResponse (..),
    newDeleteThemeResponse,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTheme' smart constructor.
data DeleteTheme = DeleteTheme'
  { -- | The unique ID of the Amplify app associated with the theme to delete.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text,
    -- | The unique ID of the theme to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteTheme_appId' - The unique ID of the Amplify app associated with the theme to delete.
--
-- 'environmentName', 'deleteTheme_environmentName' - The name of the backend environment that is a part of the Amplify app.
--
-- 'id', 'deleteTheme_id' - The unique ID of the theme to delete.
newDeleteTheme ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DeleteTheme
newDeleteTheme pAppId_ pEnvironmentName_ pId_ =
  DeleteTheme'
    { appId = pAppId_,
      environmentName = pEnvironmentName_,
      id = pId_
    }

-- | The unique ID of the Amplify app associated with the theme to delete.
deleteTheme_appId :: Lens.Lens' DeleteTheme Prelude.Text
deleteTheme_appId = Lens.lens (\DeleteTheme' {appId} -> appId) (\s@DeleteTheme' {} a -> s {appId = a} :: DeleteTheme)

-- | The name of the backend environment that is a part of the Amplify app.
deleteTheme_environmentName :: Lens.Lens' DeleteTheme Prelude.Text
deleteTheme_environmentName = Lens.lens (\DeleteTheme' {environmentName} -> environmentName) (\s@DeleteTheme' {} a -> s {environmentName = a} :: DeleteTheme)

-- | The unique ID of the theme to delete.
deleteTheme_id :: Lens.Lens' DeleteTheme Prelude.Text
deleteTheme_id = Lens.lens (\DeleteTheme' {id} -> id) (\s@DeleteTheme' {} a -> s {id = a} :: DeleteTheme)

instance Core.AWSRequest DeleteTheme where
  type AWSResponse DeleteTheme = DeleteThemeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteThemeResponse'

instance Prelude.Hashable DeleteTheme where
  hashWithSalt _salt DeleteTheme' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteTheme where
  rnf DeleteTheme' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteTheme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTheme where
  toPath DeleteTheme' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/themes/",
        Data.toBS id
      ]

instance Data.ToQuery DeleteTheme where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteThemeResponse' smart constructor.
data DeleteThemeResponse = DeleteThemeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteThemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteThemeResponse ::
  DeleteThemeResponse
newDeleteThemeResponse = DeleteThemeResponse'

instance Prelude.NFData DeleteThemeResponse where
  rnf _ = ()
