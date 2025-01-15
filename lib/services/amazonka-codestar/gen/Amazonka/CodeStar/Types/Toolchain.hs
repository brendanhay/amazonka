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
-- Module      : Amazonka.CodeStar.Types.Toolchain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.Toolchain where

import Amazonka.CodeStar.Types.ToolchainSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The toolchain template file provided with the project request. AWS
-- CodeStar uses the template to provision the toolchain stack in AWS
-- CloudFormation.
--
-- /See:/ 'newToolchain' smart constructor.
data Toolchain = Toolchain'
  { -- | The service role ARN for AWS CodeStar to use for the toolchain template
    -- during stack provisioning.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The list of parameter overrides to be passed into the toolchain template
    -- during stack provisioning, if any.
    stackParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The Amazon S3 location where the toolchain template file provided with
    -- the project request is stored. AWS CodeStar retrieves the file during
    -- project creation.
    source :: ToolchainSource
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Toolchain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'toolchain_roleArn' - The service role ARN for AWS CodeStar to use for the toolchain template
-- during stack provisioning.
--
-- 'stackParameters', 'toolchain_stackParameters' - The list of parameter overrides to be passed into the toolchain template
-- during stack provisioning, if any.
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
    { roleArn = Prelude.Nothing,
      stackParameters = Prelude.Nothing,
      source = pSource_
    }

-- | The service role ARN for AWS CodeStar to use for the toolchain template
-- during stack provisioning.
toolchain_roleArn :: Lens.Lens' Toolchain (Prelude.Maybe Prelude.Text)
toolchain_roleArn = Lens.lens (\Toolchain' {roleArn} -> roleArn) (\s@Toolchain' {} a -> s {roleArn = a} :: Toolchain)

-- | The list of parameter overrides to be passed into the toolchain template
-- during stack provisioning, if any.
toolchain_stackParameters :: Lens.Lens' Toolchain (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
toolchain_stackParameters = Lens.lens (\Toolchain' {stackParameters} -> stackParameters) (\s@Toolchain' {} a -> s {stackParameters = a} :: Toolchain) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location where the toolchain template file provided with
-- the project request is stored. AWS CodeStar retrieves the file during
-- project creation.
toolchain_source :: Lens.Lens' Toolchain ToolchainSource
toolchain_source = Lens.lens (\Toolchain' {source} -> source) (\s@Toolchain' {} a -> s {source = a} :: Toolchain)

instance Prelude.Hashable Toolchain where
  hashWithSalt _salt Toolchain' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` stackParameters
      `Prelude.hashWithSalt` source

instance Prelude.NFData Toolchain where
  rnf Toolchain' {..} =
    Prelude.rnf roleArn `Prelude.seq`
      Prelude.rnf stackParameters `Prelude.seq`
        Prelude.rnf source

instance Data.ToJSON Toolchain where
  toJSON Toolchain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("roleArn" Data..=) Prelude.<$> roleArn,
            ("stackParameters" Data..=)
              Prelude.<$> stackParameters,
            Prelude.Just ("source" Data..= source)
          ]
      )
