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
-- Module      : Amazonka.QuickSight.DescribeTheme
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a theme.
module Amazonka.QuickSight.DescribeTheme
  ( -- * Creating a Request
    DescribeTheme (..),
    newDescribeTheme,

    -- * Request Lenses
    describeTheme_versionNumber,
    describeTheme_aliasName,
    describeTheme_awsAccountId,
    describeTheme_themeId,

    -- * Destructuring the Response
    DescribeThemeResponse (..),
    newDescribeThemeResponse,

    -- * Response Lenses
    describeThemeResponse_requestId,
    describeThemeResponse_theme,
    describeThemeResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTheme' smart constructor.
data DescribeTheme = DescribeTheme'
  { -- | The version number for the version to describe. If a @VersionNumber@
    -- parameter value isn\'t provided, the latest version of the theme is
    -- described.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The alias of the theme that you want to describe. If you name a specific
    -- alias, you describe the version that the alias points to. You can
    -- specify the latest version of the theme by providing the keyword
    -- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
    -- doesn\'t apply to themes.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the theme that
    -- you\'re describing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the theme.
    themeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTheme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionNumber', 'describeTheme_versionNumber' - The version number for the version to describe. If a @VersionNumber@
-- parameter value isn\'t provided, the latest version of the theme is
-- described.
--
-- 'aliasName', 'describeTheme_aliasName' - The alias of the theme that you want to describe. If you name a specific
-- alias, you describe the version that the alias points to. You can
-- specify the latest version of the theme by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to themes.
--
-- 'awsAccountId', 'describeTheme_awsAccountId' - The ID of the Amazon Web Services account that contains the theme that
-- you\'re describing.
--
-- 'themeId', 'describeTheme_themeId' - The ID for the theme.
newDescribeTheme ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  DescribeTheme
newDescribeTheme pAwsAccountId_ pThemeId_ =
  DescribeTheme'
    { versionNumber = Prelude.Nothing,
      aliasName = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      themeId = pThemeId_
    }

-- | The version number for the version to describe. If a @VersionNumber@
-- parameter value isn\'t provided, the latest version of the theme is
-- described.
describeTheme_versionNumber :: Lens.Lens' DescribeTheme (Prelude.Maybe Prelude.Natural)
describeTheme_versionNumber = Lens.lens (\DescribeTheme' {versionNumber} -> versionNumber) (\s@DescribeTheme' {} a -> s {versionNumber = a} :: DescribeTheme)

-- | The alias of the theme that you want to describe. If you name a specific
-- alias, you describe the version that the alias points to. You can
-- specify the latest version of the theme by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to themes.
describeTheme_aliasName :: Lens.Lens' DescribeTheme (Prelude.Maybe Prelude.Text)
describeTheme_aliasName = Lens.lens (\DescribeTheme' {aliasName} -> aliasName) (\s@DescribeTheme' {} a -> s {aliasName = a} :: DescribeTheme)

-- | The ID of the Amazon Web Services account that contains the theme that
-- you\'re describing.
describeTheme_awsAccountId :: Lens.Lens' DescribeTheme Prelude.Text
describeTheme_awsAccountId = Lens.lens (\DescribeTheme' {awsAccountId} -> awsAccountId) (\s@DescribeTheme' {} a -> s {awsAccountId = a} :: DescribeTheme)

-- | The ID for the theme.
describeTheme_themeId :: Lens.Lens' DescribeTheme Prelude.Text
describeTheme_themeId = Lens.lens (\DescribeTheme' {themeId} -> themeId) (\s@DescribeTheme' {} a -> s {themeId = a} :: DescribeTheme)

instance Core.AWSRequest DescribeTheme where
  type
    AWSResponse DescribeTheme =
      DescribeThemeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThemeResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Theme")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTheme where
  hashWithSalt _salt DescribeTheme' {..} =
    _salt `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` themeId

instance Prelude.NFData DescribeTheme where
  rnf DescribeTheme' {..} =
    Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf themeId

instance Data.ToHeaders DescribeTheme where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTheme where
  toPath DescribeTheme' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/themes/",
        Data.toBS themeId
      ]

instance Data.ToQuery DescribeTheme where
  toQuery DescribeTheme' {..} =
    Prelude.mconcat
      [ "version-number" Data.=: versionNumber,
        "alias-name" Data.=: aliasName
      ]

-- | /See:/ 'newDescribeThemeResponse' smart constructor.
data DescribeThemeResponse = DescribeThemeResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The information about the theme that you are describing.
    theme :: Prelude.Maybe Theme,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeThemeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeThemeResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'theme', 'describeThemeResponse_theme' - The information about the theme that you are describing.
--
-- 'status', 'describeThemeResponse_status' - The HTTP status of the request.
newDescribeThemeResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeThemeResponse
newDescribeThemeResponse pStatus_ =
  DescribeThemeResponse'
    { requestId = Prelude.Nothing,
      theme = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeThemeResponse_requestId :: Lens.Lens' DescribeThemeResponse (Prelude.Maybe Prelude.Text)
describeThemeResponse_requestId = Lens.lens (\DescribeThemeResponse' {requestId} -> requestId) (\s@DescribeThemeResponse' {} a -> s {requestId = a} :: DescribeThemeResponse)

-- | The information about the theme that you are describing.
describeThemeResponse_theme :: Lens.Lens' DescribeThemeResponse (Prelude.Maybe Theme)
describeThemeResponse_theme = Lens.lens (\DescribeThemeResponse' {theme} -> theme) (\s@DescribeThemeResponse' {} a -> s {theme = a} :: DescribeThemeResponse)

-- | The HTTP status of the request.
describeThemeResponse_status :: Lens.Lens' DescribeThemeResponse Prelude.Int
describeThemeResponse_status = Lens.lens (\DescribeThemeResponse' {status} -> status) (\s@DescribeThemeResponse' {} a -> s {status = a} :: DescribeThemeResponse)

instance Prelude.NFData DescribeThemeResponse where
  rnf DescribeThemeResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf theme
      `Prelude.seq` Prelude.rnf status
