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
-- Module      : Network.AWS.Lambda.UpdateCodeSigningConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the code signing configuration. Changes to the code signing
-- configuration take effect the next time a user tries to deploy a code
-- package to the function.
module Network.AWS.Lambda.UpdateCodeSigningConfig
  ( -- * Creating a Request
    UpdateCodeSigningConfig (..),
    newUpdateCodeSigningConfig,

    -- * Request Lenses
    updateCodeSigningConfig_allowedPublishers,
    updateCodeSigningConfig_description,
    updateCodeSigningConfig_codeSigningPolicies,
    updateCodeSigningConfig_codeSigningConfigArn,

    -- * Destructuring the Response
    UpdateCodeSigningConfigResponse (..),
    newUpdateCodeSigningConfigResponse,

    -- * Response Lenses
    updateCodeSigningConfigResponse_httpStatus,
    updateCodeSigningConfigResponse_codeSigningConfig,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCodeSigningConfig' smart constructor.
data UpdateCodeSigningConfig = UpdateCodeSigningConfig'
  { -- | Signing profiles for this code signing configuration.
    allowedPublishers :: Prelude.Maybe AllowedPublishers,
    -- | Descriptive name for this code signing configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The code signing policy.
    codeSigningPolicies :: Prelude.Maybe CodeSigningPolicies,
    -- | The The Amazon Resource Name (ARN) of the code signing configuration.
    codeSigningConfigArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCodeSigningConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedPublishers', 'updateCodeSigningConfig_allowedPublishers' - Signing profiles for this code signing configuration.
--
-- 'description', 'updateCodeSigningConfig_description' - Descriptive name for this code signing configuration.
--
-- 'codeSigningPolicies', 'updateCodeSigningConfig_codeSigningPolicies' - The code signing policy.
--
-- 'codeSigningConfigArn', 'updateCodeSigningConfig_codeSigningConfigArn' - The The Amazon Resource Name (ARN) of the code signing configuration.
newUpdateCodeSigningConfig ::
  -- | 'codeSigningConfigArn'
  Prelude.Text ->
  UpdateCodeSigningConfig
newUpdateCodeSigningConfig pCodeSigningConfigArn_ =
  UpdateCodeSigningConfig'
    { allowedPublishers =
        Prelude.Nothing,
      description = Prelude.Nothing,
      codeSigningPolicies = Prelude.Nothing,
      codeSigningConfigArn = pCodeSigningConfigArn_
    }

-- | Signing profiles for this code signing configuration.
updateCodeSigningConfig_allowedPublishers :: Lens.Lens' UpdateCodeSigningConfig (Prelude.Maybe AllowedPublishers)
updateCodeSigningConfig_allowedPublishers = Lens.lens (\UpdateCodeSigningConfig' {allowedPublishers} -> allowedPublishers) (\s@UpdateCodeSigningConfig' {} a -> s {allowedPublishers = a} :: UpdateCodeSigningConfig)

-- | Descriptive name for this code signing configuration.
updateCodeSigningConfig_description :: Lens.Lens' UpdateCodeSigningConfig (Prelude.Maybe Prelude.Text)
updateCodeSigningConfig_description = Lens.lens (\UpdateCodeSigningConfig' {description} -> description) (\s@UpdateCodeSigningConfig' {} a -> s {description = a} :: UpdateCodeSigningConfig)

-- | The code signing policy.
updateCodeSigningConfig_codeSigningPolicies :: Lens.Lens' UpdateCodeSigningConfig (Prelude.Maybe CodeSigningPolicies)
updateCodeSigningConfig_codeSigningPolicies = Lens.lens (\UpdateCodeSigningConfig' {codeSigningPolicies} -> codeSigningPolicies) (\s@UpdateCodeSigningConfig' {} a -> s {codeSigningPolicies = a} :: UpdateCodeSigningConfig)

-- | The The Amazon Resource Name (ARN) of the code signing configuration.
updateCodeSigningConfig_codeSigningConfigArn :: Lens.Lens' UpdateCodeSigningConfig Prelude.Text
updateCodeSigningConfig_codeSigningConfigArn = Lens.lens (\UpdateCodeSigningConfig' {codeSigningConfigArn} -> codeSigningConfigArn) (\s@UpdateCodeSigningConfig' {} a -> s {codeSigningConfigArn = a} :: UpdateCodeSigningConfig)

instance Prelude.AWSRequest UpdateCodeSigningConfig where
  type
    Rs UpdateCodeSigningConfig =
      UpdateCodeSigningConfigResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCodeSigningConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "CodeSigningConfig")
      )

instance Prelude.Hashable UpdateCodeSigningConfig

instance Prelude.NFData UpdateCodeSigningConfig

instance Prelude.ToHeaders UpdateCodeSigningConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateCodeSigningConfig where
  toJSON UpdateCodeSigningConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AllowedPublishers" Prelude..=)
              Prelude.<$> allowedPublishers,
            ("Description" Prelude..=) Prelude.<$> description,
            ("CodeSigningPolicies" Prelude..=)
              Prelude.<$> codeSigningPolicies
          ]
      )

instance Prelude.ToPath UpdateCodeSigningConfig where
  toPath UpdateCodeSigningConfig' {..} =
    Prelude.mconcat
      [ "/2020-04-22/code-signing-configs/",
        Prelude.toBS codeSigningConfigArn
      ]

instance Prelude.ToQuery UpdateCodeSigningConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCodeSigningConfigResponse' smart constructor.
data UpdateCodeSigningConfigResponse = UpdateCodeSigningConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The code signing configuration
    codeSigningConfig :: CodeSigningConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCodeSigningConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCodeSigningConfigResponse_httpStatus' - The response's http status code.
--
-- 'codeSigningConfig', 'updateCodeSigningConfigResponse_codeSigningConfig' - The code signing configuration
newUpdateCodeSigningConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'codeSigningConfig'
  CodeSigningConfig ->
  UpdateCodeSigningConfigResponse
newUpdateCodeSigningConfigResponse
  pHttpStatus_
  pCodeSigningConfig_ =
    UpdateCodeSigningConfigResponse'
      { httpStatus =
          pHttpStatus_,
        codeSigningConfig = pCodeSigningConfig_
      }

-- | The response's http status code.
updateCodeSigningConfigResponse_httpStatus :: Lens.Lens' UpdateCodeSigningConfigResponse Prelude.Int
updateCodeSigningConfigResponse_httpStatus = Lens.lens (\UpdateCodeSigningConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateCodeSigningConfigResponse' {} a -> s {httpStatus = a} :: UpdateCodeSigningConfigResponse)

-- | The code signing configuration
updateCodeSigningConfigResponse_codeSigningConfig :: Lens.Lens' UpdateCodeSigningConfigResponse CodeSigningConfig
updateCodeSigningConfigResponse_codeSigningConfig = Lens.lens (\UpdateCodeSigningConfigResponse' {codeSigningConfig} -> codeSigningConfig) (\s@UpdateCodeSigningConfigResponse' {} a -> s {codeSigningConfig = a} :: UpdateCodeSigningConfigResponse)

instance
  Prelude.NFData
    UpdateCodeSigningConfigResponse
