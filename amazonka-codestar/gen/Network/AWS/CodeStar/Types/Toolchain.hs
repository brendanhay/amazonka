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
-- Module      : Network.AWS.CodeStar.Types.Toolchain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Toolchain where

import Network.AWS.CodeStar.Types.ToolchainSource
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The toolchain template file provided with the project request. AWS
-- CodeStar uses the template to provision the toolchain stack in AWS
-- CloudFormation.
--
-- /See:/ 'newToolchain' smart constructor.
data Toolchain = Toolchain'
  { -- | The list of parameter overrides to be passed into the toolchain template
    -- during stack provisioning, if any.
    stackParameters :: Core.Maybe (Core.HashMap Core.Text (Core.Sensitive Core.Text)),
    -- | The service role ARN for AWS CodeStar to use for the toolchain template
    -- during stack provisioning.
    roleArn :: Core.Maybe Core.Text,
    -- | The Amazon S3 location where the toolchain template file provided with
    -- the project request is stored. AWS CodeStar retrieves the file during
    -- project creation.
    source :: ToolchainSource
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'Toolchain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackParameters', 'toolchain_stackParameters' - The list of parameter overrides to be passed into the toolchain template
-- during stack provisioning, if any.
--
-- 'roleArn', 'toolchain_roleArn' - The service role ARN for AWS CodeStar to use for the toolchain template
-- during stack provisioning.
--
-- 'source', 'toolchain_source' - The Amazon S3 location where the toolchain template file provided with
-- the project request is stored. AWS CodeStar retrieves the file during
-- project creation.
newToolchain ::
  -- | 'source'
  ToolchainSource ->
  Toolchain
newToolchain pSource_ =
  Toolchain'
    { stackParameters = Core.Nothing,
      roleArn = Core.Nothing,
      source = pSource_
    }

-- | The list of parameter overrides to be passed into the toolchain template
-- during stack provisioning, if any.
toolchain_stackParameters :: Lens.Lens' Toolchain (Core.Maybe (Core.HashMap Core.Text Core.Text))
toolchain_stackParameters = Lens.lens (\Toolchain' {stackParameters} -> stackParameters) (\s@Toolchain' {} a -> s {stackParameters = a} :: Toolchain) Core.. Lens.mapping Lens._Coerce

-- | The service role ARN for AWS CodeStar to use for the toolchain template
-- during stack provisioning.
toolchain_roleArn :: Lens.Lens' Toolchain (Core.Maybe Core.Text)
toolchain_roleArn = Lens.lens (\Toolchain' {roleArn} -> roleArn) (\s@Toolchain' {} a -> s {roleArn = a} :: Toolchain)

-- | The Amazon S3 location where the toolchain template file provided with
-- the project request is stored. AWS CodeStar retrieves the file during
-- project creation.
toolchain_source :: Lens.Lens' Toolchain ToolchainSource
toolchain_source = Lens.lens (\Toolchain' {source} -> source) (\s@Toolchain' {} a -> s {source = a} :: Toolchain)

instance Core.Hashable Toolchain

instance Core.NFData Toolchain

instance Core.ToJSON Toolchain where
  toJSON Toolchain' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stackParameters" Core..=)
              Core.<$> stackParameters,
            ("roleArn" Core..=) Core.<$> roleArn,
            Core.Just ("source" Core..= source)
          ]
      )
