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
-- Module      : Amazonka.QuickSight.DeleteTheme
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a theme.
module Amazonka.QuickSight.DeleteTheme
  ( -- * Creating a Request
    DeleteTheme (..),
    newDeleteTheme,

    -- * Request Lenses
    deleteTheme_versionNumber,
    deleteTheme_awsAccountId,
    deleteTheme_themeId,

    -- * Destructuring the Response
    DeleteThemeResponse (..),
    newDeleteThemeResponse,

    -- * Response Lenses
    deleteThemeResponse_requestId,
    deleteThemeResponse_arn,
    deleteThemeResponse_themeId,
    deleteThemeResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTheme' smart constructor.
data DeleteTheme = DeleteTheme'
  { -- | The version of the theme that you want to delete.
    --
    -- __Note:__ If you don\'t provide a version number, you\'re using this
    -- call to @DeleteTheme@ to delete all versions of the theme.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains the theme that
    -- you\'re deleting.
    awsAccountId :: Prelude.Text,
    -- | An ID for the theme that you want to delete.
    themeId :: Prelude.Text
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
-- 'versionNumber', 'deleteTheme_versionNumber' - The version of the theme that you want to delete.
--
-- __Note:__ If you don\'t provide a version number, you\'re using this
-- call to @DeleteTheme@ to delete all versions of the theme.
--
-- 'awsAccountId', 'deleteTheme_awsAccountId' - The ID of the Amazon Web Services account that contains the theme that
-- you\'re deleting.
--
-- 'themeId', 'deleteTheme_themeId' - An ID for the theme that you want to delete.
newDeleteTheme ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  DeleteTheme
newDeleteTheme pAwsAccountId_ pThemeId_ =
  DeleteTheme'
    { versionNumber = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      themeId = pThemeId_
    }

-- | The version of the theme that you want to delete.
--
-- __Note:__ If you don\'t provide a version number, you\'re using this
-- call to @DeleteTheme@ to delete all versions of the theme.
deleteTheme_versionNumber :: Lens.Lens' DeleteTheme (Prelude.Maybe Prelude.Natural)
deleteTheme_versionNumber = Lens.lens (\DeleteTheme' {versionNumber} -> versionNumber) (\s@DeleteTheme' {} a -> s {versionNumber = a} :: DeleteTheme)

-- | The ID of the Amazon Web Services account that contains the theme that
-- you\'re deleting.
deleteTheme_awsAccountId :: Lens.Lens' DeleteTheme Prelude.Text
deleteTheme_awsAccountId = Lens.lens (\DeleteTheme' {awsAccountId} -> awsAccountId) (\s@DeleteTheme' {} a -> s {awsAccountId = a} :: DeleteTheme)

-- | An ID for the theme that you want to delete.
deleteTheme_themeId :: Lens.Lens' DeleteTheme Prelude.Text
deleteTheme_themeId = Lens.lens (\DeleteTheme' {themeId} -> themeId) (\s@DeleteTheme' {} a -> s {themeId = a} :: DeleteTheme)

instance Core.AWSRequest DeleteTheme where
  type AWSResponse DeleteTheme = DeleteThemeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteThemeResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "ThemeId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTheme where
  hashWithSalt _salt DeleteTheme' {..} =
    _salt `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` themeId

instance Prelude.NFData DeleteTheme where
  rnf DeleteTheme' {..} =
    Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf themeId

instance Data.ToHeaders DeleteTheme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTheme where
  toPath DeleteTheme' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/themes/",
        Data.toBS themeId
      ]

instance Data.ToQuery DeleteTheme where
  toQuery DeleteTheme' {..} =
    Prelude.mconcat
      ["version-number" Data.=: versionNumber]

-- | /See:/ 'newDeleteThemeResponse' smart constructor.
data DeleteThemeResponse = DeleteThemeResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An ID for the theme.
    themeId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteThemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteThemeResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'deleteThemeResponse_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'themeId', 'deleteThemeResponse_themeId' - An ID for the theme.
--
-- 'status', 'deleteThemeResponse_status' - The HTTP status of the request.
newDeleteThemeResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteThemeResponse
newDeleteThemeResponse pStatus_ =
  DeleteThemeResponse'
    { requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      themeId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteThemeResponse_requestId :: Lens.Lens' DeleteThemeResponse (Prelude.Maybe Prelude.Text)
deleteThemeResponse_requestId = Lens.lens (\DeleteThemeResponse' {requestId} -> requestId) (\s@DeleteThemeResponse' {} a -> s {requestId = a} :: DeleteThemeResponse)

-- | The Amazon Resource Name (ARN) of the resource.
deleteThemeResponse_arn :: Lens.Lens' DeleteThemeResponse (Prelude.Maybe Prelude.Text)
deleteThemeResponse_arn = Lens.lens (\DeleteThemeResponse' {arn} -> arn) (\s@DeleteThemeResponse' {} a -> s {arn = a} :: DeleteThemeResponse)

-- | An ID for the theme.
deleteThemeResponse_themeId :: Lens.Lens' DeleteThemeResponse (Prelude.Maybe Prelude.Text)
deleteThemeResponse_themeId = Lens.lens (\DeleteThemeResponse' {themeId} -> themeId) (\s@DeleteThemeResponse' {} a -> s {themeId = a} :: DeleteThemeResponse)

-- | The HTTP status of the request.
deleteThemeResponse_status :: Lens.Lens' DeleteThemeResponse Prelude.Int
deleteThemeResponse_status = Lens.lens (\DeleteThemeResponse' {status} -> status) (\s@DeleteThemeResponse' {} a -> s {status = a} :: DeleteThemeResponse)

instance Prelude.NFData DeleteThemeResponse where
  rnf DeleteThemeResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf themeId
      `Prelude.seq` Prelude.rnf status
