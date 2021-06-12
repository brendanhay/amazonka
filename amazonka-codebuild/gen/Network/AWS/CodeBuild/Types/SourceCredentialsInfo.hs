{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceCredentialsInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceCredentialsInfo where

import Network.AWS.CodeBuild.Types.AuthType
import Network.AWS.CodeBuild.Types.ServerType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the credentials for a GitHub, GitHub Enterprise, or
-- Bitbucket repository.
--
-- /See:/ 'newSourceCredentialsInfo' smart constructor.
data SourceCredentialsInfo = SourceCredentialsInfo'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Core.Maybe Core.Text,
    -- | The type of authentication used by the credentials. Valid options are
    -- OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
    authType :: Core.Maybe AuthType,
    -- | The type of source provider. The valid options are GITHUB,
    -- GITHUB_ENTERPRISE, or BITBUCKET.
    serverType :: Core.Maybe ServerType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SourceCredentialsInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'sourceCredentialsInfo_arn' - The Amazon Resource Name (ARN) of the token.
--
-- 'authType', 'sourceCredentialsInfo_authType' - The type of authentication used by the credentials. Valid options are
-- OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
--
-- 'serverType', 'sourceCredentialsInfo_serverType' - The type of source provider. The valid options are GITHUB,
-- GITHUB_ENTERPRISE, or BITBUCKET.
newSourceCredentialsInfo ::
  SourceCredentialsInfo
newSourceCredentialsInfo =
  SourceCredentialsInfo'
    { arn = Core.Nothing,
      authType = Core.Nothing,
      serverType = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the token.
sourceCredentialsInfo_arn :: Lens.Lens' SourceCredentialsInfo (Core.Maybe Core.Text)
sourceCredentialsInfo_arn = Lens.lens (\SourceCredentialsInfo' {arn} -> arn) (\s@SourceCredentialsInfo' {} a -> s {arn = a} :: SourceCredentialsInfo)

-- | The type of authentication used by the credentials. Valid options are
-- OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
sourceCredentialsInfo_authType :: Lens.Lens' SourceCredentialsInfo (Core.Maybe AuthType)
sourceCredentialsInfo_authType = Lens.lens (\SourceCredentialsInfo' {authType} -> authType) (\s@SourceCredentialsInfo' {} a -> s {authType = a} :: SourceCredentialsInfo)

-- | The type of source provider. The valid options are GITHUB,
-- GITHUB_ENTERPRISE, or BITBUCKET.
sourceCredentialsInfo_serverType :: Lens.Lens' SourceCredentialsInfo (Core.Maybe ServerType)
sourceCredentialsInfo_serverType = Lens.lens (\SourceCredentialsInfo' {serverType} -> serverType) (\s@SourceCredentialsInfo' {} a -> s {serverType = a} :: SourceCredentialsInfo)

instance Core.FromJSON SourceCredentialsInfo where
  parseJSON =
    Core.withObject
      "SourceCredentialsInfo"
      ( \x ->
          SourceCredentialsInfo'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "authType")
            Core.<*> (x Core..:? "serverType")
      )

instance Core.Hashable SourceCredentialsInfo

instance Core.NFData SourceCredentialsInfo
