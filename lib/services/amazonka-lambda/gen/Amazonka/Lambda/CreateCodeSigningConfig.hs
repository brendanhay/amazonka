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
-- Module      : Amazonka.Lambda.CreateCodeSigningConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a code signing configuration. A
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-codesigning.html code signing configuration>
-- defines a list of allowed signing profiles and defines the code-signing
-- validation policy (action to be taken if deployment validation checks
-- fail).
module Amazonka.Lambda.CreateCodeSigningConfig
  ( -- * Creating a Request
    CreateCodeSigningConfig (..),
    newCreateCodeSigningConfig,

    -- * Request Lenses
    createCodeSigningConfig_codeSigningPolicies,
    createCodeSigningConfig_description,
    createCodeSigningConfig_allowedPublishers,

    -- * Destructuring the Response
    CreateCodeSigningConfigResponse (..),
    newCreateCodeSigningConfigResponse,

    -- * Response Lenses
    createCodeSigningConfigResponse_httpStatus,
    createCodeSigningConfigResponse_codeSigningConfig,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCodeSigningConfig' smart constructor.
data CreateCodeSigningConfig = CreateCodeSigningConfig'
  { -- | The code signing policies define the actions to take if the validation
    -- checks fail.
    codeSigningPolicies :: Prelude.Maybe CodeSigningPolicies,
    -- | Descriptive name for this code signing configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | Signing profiles for this code signing configuration.
    allowedPublishers :: AllowedPublishers
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeSigningPolicies', 'createCodeSigningConfig_codeSigningPolicies' - The code signing policies define the actions to take if the validation
-- checks fail.
--
-- 'description', 'createCodeSigningConfig_description' - Descriptive name for this code signing configuration.
--
-- 'allowedPublishers', 'createCodeSigningConfig_allowedPublishers' - Signing profiles for this code signing configuration.
newCreateCodeSigningConfig ::
  -- | 'allowedPublishers'
  AllowedPublishers ->
  CreateCodeSigningConfig
newCreateCodeSigningConfig pAllowedPublishers_ =
  CreateCodeSigningConfig'
    { codeSigningPolicies =
        Prelude.Nothing,
      description = Prelude.Nothing,
      allowedPublishers = pAllowedPublishers_
    }

-- | The code signing policies define the actions to take if the validation
-- checks fail.
createCodeSigningConfig_codeSigningPolicies :: Lens.Lens' CreateCodeSigningConfig (Prelude.Maybe CodeSigningPolicies)
createCodeSigningConfig_codeSigningPolicies = Lens.lens (\CreateCodeSigningConfig' {codeSigningPolicies} -> codeSigningPolicies) (\s@CreateCodeSigningConfig' {} a -> s {codeSigningPolicies = a} :: CreateCodeSigningConfig)

-- | Descriptive name for this code signing configuration.
createCodeSigningConfig_description :: Lens.Lens' CreateCodeSigningConfig (Prelude.Maybe Prelude.Text)
createCodeSigningConfig_description = Lens.lens (\CreateCodeSigningConfig' {description} -> description) (\s@CreateCodeSigningConfig' {} a -> s {description = a} :: CreateCodeSigningConfig)

-- | Signing profiles for this code signing configuration.
createCodeSigningConfig_allowedPublishers :: Lens.Lens' CreateCodeSigningConfig AllowedPublishers
createCodeSigningConfig_allowedPublishers = Lens.lens (\CreateCodeSigningConfig' {allowedPublishers} -> allowedPublishers) (\s@CreateCodeSigningConfig' {} a -> s {allowedPublishers = a} :: CreateCodeSigningConfig)

instance Core.AWSRequest CreateCodeSigningConfig where
  type
    AWSResponse CreateCodeSigningConfig =
      CreateCodeSigningConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCodeSigningConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CodeSigningConfig")
      )

instance Prelude.Hashable CreateCodeSigningConfig where
  hashWithSalt _salt CreateCodeSigningConfig' {..} =
    _salt
      `Prelude.hashWithSalt` codeSigningPolicies
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` allowedPublishers

instance Prelude.NFData CreateCodeSigningConfig where
  rnf CreateCodeSigningConfig' {..} =
    Prelude.rnf codeSigningPolicies
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf allowedPublishers

instance Data.ToHeaders CreateCodeSigningConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateCodeSigningConfig where
  toJSON CreateCodeSigningConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CodeSigningPolicies" Data..=)
              Prelude.<$> codeSigningPolicies,
            ("Description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("AllowedPublishers" Data..= allowedPublishers)
          ]
      )

instance Data.ToPath CreateCodeSigningConfig where
  toPath =
    Prelude.const "/2020-04-22/code-signing-configs/"

instance Data.ToQuery CreateCodeSigningConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCodeSigningConfigResponse' smart constructor.
data CreateCodeSigningConfigResponse = CreateCodeSigningConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The code signing configuration.
    codeSigningConfig :: CodeSigningConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCodeSigningConfigResponse_httpStatus' - The response's http status code.
--
-- 'codeSigningConfig', 'createCodeSigningConfigResponse_codeSigningConfig' - The code signing configuration.
newCreateCodeSigningConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'codeSigningConfig'
  CodeSigningConfig ->
  CreateCodeSigningConfigResponse
newCreateCodeSigningConfigResponse
  pHttpStatus_
  pCodeSigningConfig_ =
    CreateCodeSigningConfigResponse'
      { httpStatus =
          pHttpStatus_,
        codeSigningConfig = pCodeSigningConfig_
      }

-- | The response's http status code.
createCodeSigningConfigResponse_httpStatus :: Lens.Lens' CreateCodeSigningConfigResponse Prelude.Int
createCodeSigningConfigResponse_httpStatus = Lens.lens (\CreateCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@CreateCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: CreateCodeSigningConfigResponse)

-- | The code signing configuration.
createCodeSigningConfigResponse_codeSigningConfig :: Lens.Lens' CreateCodeSigningConfigResponse CodeSigningConfig
createCodeSigningConfigResponse_codeSigningConfig = Lens.lens (\CreateCodeSigningConfigResponse' {codeSigningConfig} -> codeSigningConfig) (\s@CreateCodeSigningConfigResponse' {} a -> s {codeSigningConfig = a} :: CreateCodeSigningConfigResponse)

instance
  Prelude.NFData
    CreateCodeSigningConfigResponse
  where
  rnf CreateCodeSigningConfigResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf codeSigningConfig
