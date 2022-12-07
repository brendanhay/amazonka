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
-- Module      : Amazonka.QuickSight.CreateThemeAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a theme alias for a theme.
module Amazonka.QuickSight.CreateThemeAlias
  ( -- * Creating a Request
    CreateThemeAlias (..),
    newCreateThemeAlias,

    -- * Request Lenses
    createThemeAlias_awsAccountId,
    createThemeAlias_themeId,
    createThemeAlias_aliasName,
    createThemeAlias_themeVersionNumber,

    -- * Destructuring the Response
    CreateThemeAliasResponse (..),
    newCreateThemeAliasResponse,

    -- * Response Lenses
    createThemeAliasResponse_requestId,
    createThemeAliasResponse_themeAlias,
    createThemeAliasResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateThemeAlias' smart constructor.
data CreateThemeAlias = CreateThemeAlias'
  { -- | The ID of the Amazon Web Services account that contains the theme for
    -- the new theme alias.
    awsAccountId :: Prelude.Text,
    -- | An ID for the theme alias.
    themeId :: Prelude.Text,
    -- | The name that you want to give to the theme alias that you are creating.
    -- The alias name can\'t begin with a @$@. Alias names that start with @$@
    -- are reserved by Amazon QuickSight.
    aliasName :: Prelude.Text,
    -- | The version number of the theme.
    themeVersionNumber :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThemeAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'createThemeAlias_awsAccountId' - The ID of the Amazon Web Services account that contains the theme for
-- the new theme alias.
--
-- 'themeId', 'createThemeAlias_themeId' - An ID for the theme alias.
--
-- 'aliasName', 'createThemeAlias_aliasName' - The name that you want to give to the theme alias that you are creating.
-- The alias name can\'t begin with a @$@. Alias names that start with @$@
-- are reserved by Amazon QuickSight.
--
-- 'themeVersionNumber', 'createThemeAlias_themeVersionNumber' - The version number of the theme.
newCreateThemeAlias ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  -- | 'aliasName'
  Prelude.Text ->
  -- | 'themeVersionNumber'
  Prelude.Natural ->
  CreateThemeAlias
newCreateThemeAlias
  pAwsAccountId_
  pThemeId_
  pAliasName_
  pThemeVersionNumber_ =
    CreateThemeAlias'
      { awsAccountId = pAwsAccountId_,
        themeId = pThemeId_,
        aliasName = pAliasName_,
        themeVersionNumber = pThemeVersionNumber_
      }

-- | The ID of the Amazon Web Services account that contains the theme for
-- the new theme alias.
createThemeAlias_awsAccountId :: Lens.Lens' CreateThemeAlias Prelude.Text
createThemeAlias_awsAccountId = Lens.lens (\CreateThemeAlias' {awsAccountId} -> awsAccountId) (\s@CreateThemeAlias' {} a -> s {awsAccountId = a} :: CreateThemeAlias)

-- | An ID for the theme alias.
createThemeAlias_themeId :: Lens.Lens' CreateThemeAlias Prelude.Text
createThemeAlias_themeId = Lens.lens (\CreateThemeAlias' {themeId} -> themeId) (\s@CreateThemeAlias' {} a -> s {themeId = a} :: CreateThemeAlias)

-- | The name that you want to give to the theme alias that you are creating.
-- The alias name can\'t begin with a @$@. Alias names that start with @$@
-- are reserved by Amazon QuickSight.
createThemeAlias_aliasName :: Lens.Lens' CreateThemeAlias Prelude.Text
createThemeAlias_aliasName = Lens.lens (\CreateThemeAlias' {aliasName} -> aliasName) (\s@CreateThemeAlias' {} a -> s {aliasName = a} :: CreateThemeAlias)

-- | The version number of the theme.
createThemeAlias_themeVersionNumber :: Lens.Lens' CreateThemeAlias Prelude.Natural
createThemeAlias_themeVersionNumber = Lens.lens (\CreateThemeAlias' {themeVersionNumber} -> themeVersionNumber) (\s@CreateThemeAlias' {} a -> s {themeVersionNumber = a} :: CreateThemeAlias)

instance Core.AWSRequest CreateThemeAlias where
  type
    AWSResponse CreateThemeAlias =
      CreateThemeAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThemeAliasResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ThemeAlias")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThemeAlias where
  hashWithSalt _salt CreateThemeAlias' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` themeId
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` themeVersionNumber

instance Prelude.NFData CreateThemeAlias where
  rnf CreateThemeAlias' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf themeId
      `Prelude.seq` Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf themeVersionNumber

instance Data.ToHeaders CreateThemeAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateThemeAlias where
  toJSON CreateThemeAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ThemeVersionNumber" Data..= themeVersionNumber)
          ]
      )

instance Data.ToPath CreateThemeAlias where
  toPath CreateThemeAlias' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/themes/",
        Data.toBS themeId,
        "/aliases/",
        Data.toBS aliasName
      ]

instance Data.ToQuery CreateThemeAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateThemeAliasResponse' smart constructor.
data CreateThemeAliasResponse = CreateThemeAliasResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Information about the theme alias.
    themeAlias :: Prelude.Maybe ThemeAlias,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThemeAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createThemeAliasResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'themeAlias', 'createThemeAliasResponse_themeAlias' - Information about the theme alias.
--
-- 'status', 'createThemeAliasResponse_status' - The HTTP status of the request.
newCreateThemeAliasResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateThemeAliasResponse
newCreateThemeAliasResponse pStatus_ =
  CreateThemeAliasResponse'
    { requestId =
        Prelude.Nothing,
      themeAlias = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
createThemeAliasResponse_requestId :: Lens.Lens' CreateThemeAliasResponse (Prelude.Maybe Prelude.Text)
createThemeAliasResponse_requestId = Lens.lens (\CreateThemeAliasResponse' {requestId} -> requestId) (\s@CreateThemeAliasResponse' {} a -> s {requestId = a} :: CreateThemeAliasResponse)

-- | Information about the theme alias.
createThemeAliasResponse_themeAlias :: Lens.Lens' CreateThemeAliasResponse (Prelude.Maybe ThemeAlias)
createThemeAliasResponse_themeAlias = Lens.lens (\CreateThemeAliasResponse' {themeAlias} -> themeAlias) (\s@CreateThemeAliasResponse' {} a -> s {themeAlias = a} :: CreateThemeAliasResponse)

-- | The HTTP status of the request.
createThemeAliasResponse_status :: Lens.Lens' CreateThemeAliasResponse Prelude.Int
createThemeAliasResponse_status = Lens.lens (\CreateThemeAliasResponse' {status} -> status) (\s@CreateThemeAliasResponse' {} a -> s {status = a} :: CreateThemeAliasResponse)

instance Prelude.NFData CreateThemeAliasResponse where
  rnf CreateThemeAliasResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf themeAlias
      `Prelude.seq` Prelude.rnf status
