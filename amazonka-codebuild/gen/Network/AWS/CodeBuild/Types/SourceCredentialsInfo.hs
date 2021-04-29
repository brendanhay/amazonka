{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the credentials for a GitHub, GitHub Enterprise, or
-- Bitbucket repository.
--
-- /See:/ 'newSourceCredentialsInfo' smart constructor.
data SourceCredentialsInfo = SourceCredentialsInfo'
  { -- | The Amazon Resource Name (ARN) of the token.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of authentication used by the credentials. Valid options are
    -- OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
    authType :: Prelude.Maybe AuthType,
    -- | The type of source provider. The valid options are GITHUB,
    -- GITHUB_ENTERPRISE, or BITBUCKET.
    serverType :: Prelude.Maybe ServerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { arn = Prelude.Nothing,
      authType = Prelude.Nothing,
      serverType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the token.
sourceCredentialsInfo_arn :: Lens.Lens' SourceCredentialsInfo (Prelude.Maybe Prelude.Text)
sourceCredentialsInfo_arn = Lens.lens (\SourceCredentialsInfo' {arn} -> arn) (\s@SourceCredentialsInfo' {} a -> s {arn = a} :: SourceCredentialsInfo)

-- | The type of authentication used by the credentials. Valid options are
-- OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
sourceCredentialsInfo_authType :: Lens.Lens' SourceCredentialsInfo (Prelude.Maybe AuthType)
sourceCredentialsInfo_authType = Lens.lens (\SourceCredentialsInfo' {authType} -> authType) (\s@SourceCredentialsInfo' {} a -> s {authType = a} :: SourceCredentialsInfo)

-- | The type of source provider. The valid options are GITHUB,
-- GITHUB_ENTERPRISE, or BITBUCKET.
sourceCredentialsInfo_serverType :: Lens.Lens' SourceCredentialsInfo (Prelude.Maybe ServerType)
sourceCredentialsInfo_serverType = Lens.lens (\SourceCredentialsInfo' {serverType} -> serverType) (\s@SourceCredentialsInfo' {} a -> s {serverType = a} :: SourceCredentialsInfo)

instance Prelude.FromJSON SourceCredentialsInfo where
  parseJSON =
    Prelude.withObject
      "SourceCredentialsInfo"
      ( \x ->
          SourceCredentialsInfo'
            Prelude.<$> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "authType")
            Prelude.<*> (x Prelude..:? "serverType")
      )

instance Prelude.Hashable SourceCredentialsInfo

instance Prelude.NFData SourceCredentialsInfo
