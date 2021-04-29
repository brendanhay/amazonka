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
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentVariable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentVariable where

import Network.AWS.CodeBuild.Types.EnvironmentVariableType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an environment variable for a build project or a
-- build.
--
-- /See:/ 'newEnvironmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { -- | The type of environment variable. Valid values include:
    --
    -- -   @PARAMETER_STORE@: An environment variable stored in Amazon EC2
    --     Systems Manager Parameter Store. To learn how to specify a parameter
    --     store environment variable, see
    --     <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.parameter-store env\/parameter-store>
    --     in the /AWS CodeBuild User Guide/.
    --
    -- -   @PLAINTEXT@: An environment variable in plain text format. This is
    --     the default value.
    --
    -- -   @SECRETS_MANAGER@: An environment variable stored in AWS Secrets
    --     Manager. To learn how to specify a secrets manager environment
    --     variable, see
    --     <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.secrets-manager env\/secrets-manager>
    --     in the /AWS CodeBuild User Guide/.
    type' :: Prelude.Maybe EnvironmentVariableType,
    -- | The name or key of the environment variable.
    name :: Prelude.Text,
    -- | The value of the environment variable.
    --
    -- We strongly discourage the use of @PLAINTEXT@ environment variables to
    -- store sensitive values, especially AWS secret key IDs and secret access
    -- keys. @PLAINTEXT@ environment variables can be displayed in plain text
    -- using the AWS CodeBuild console and the AWS Command Line Interface (AWS
    -- CLI). For sensitive values, we recommend you use an environment variable
    -- of type @PARAMETER_STORE@ or @SECRETS_MANAGER@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentVariable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'environmentVariable_type' - The type of environment variable. Valid values include:
--
-- -   @PARAMETER_STORE@: An environment variable stored in Amazon EC2
--     Systems Manager Parameter Store. To learn how to specify a parameter
--     store environment variable, see
--     <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.parameter-store env\/parameter-store>
--     in the /AWS CodeBuild User Guide/.
--
-- -   @PLAINTEXT@: An environment variable in plain text format. This is
--     the default value.
--
-- -   @SECRETS_MANAGER@: An environment variable stored in AWS Secrets
--     Manager. To learn how to specify a secrets manager environment
--     variable, see
--     <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.secrets-manager env\/secrets-manager>
--     in the /AWS CodeBuild User Guide/.
--
-- 'name', 'environmentVariable_name' - The name or key of the environment variable.
--
-- 'value', 'environmentVariable_value' - The value of the environment variable.
--
-- We strongly discourage the use of @PLAINTEXT@ environment variables to
-- store sensitive values, especially AWS secret key IDs and secret access
-- keys. @PLAINTEXT@ environment variables can be displayed in plain text
-- using the AWS CodeBuild console and the AWS Command Line Interface (AWS
-- CLI). For sensitive values, we recommend you use an environment variable
-- of type @PARAMETER_STORE@ or @SECRETS_MANAGER@.
newEnvironmentVariable ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  EnvironmentVariable
newEnvironmentVariable pName_ pValue_ =
  EnvironmentVariable'
    { type' = Prelude.Nothing,
      name = pName_,
      value = pValue_
    }

-- | The type of environment variable. Valid values include:
--
-- -   @PARAMETER_STORE@: An environment variable stored in Amazon EC2
--     Systems Manager Parameter Store. To learn how to specify a parameter
--     store environment variable, see
--     <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.parameter-store env\/parameter-store>
--     in the /AWS CodeBuild User Guide/.
--
-- -   @PLAINTEXT@: An environment variable in plain text format. This is
--     the default value.
--
-- -   @SECRETS_MANAGER@: An environment variable stored in AWS Secrets
--     Manager. To learn how to specify a secrets manager environment
--     variable, see
--     <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.secrets-manager env\/secrets-manager>
--     in the /AWS CodeBuild User Guide/.
environmentVariable_type :: Lens.Lens' EnvironmentVariable (Prelude.Maybe EnvironmentVariableType)
environmentVariable_type = Lens.lens (\EnvironmentVariable' {type'} -> type') (\s@EnvironmentVariable' {} a -> s {type' = a} :: EnvironmentVariable)

-- | The name or key of the environment variable.
environmentVariable_name :: Lens.Lens' EnvironmentVariable Prelude.Text
environmentVariable_name = Lens.lens (\EnvironmentVariable' {name} -> name) (\s@EnvironmentVariable' {} a -> s {name = a} :: EnvironmentVariable)

-- | The value of the environment variable.
--
-- We strongly discourage the use of @PLAINTEXT@ environment variables to
-- store sensitive values, especially AWS secret key IDs and secret access
-- keys. @PLAINTEXT@ environment variables can be displayed in plain text
-- using the AWS CodeBuild console and the AWS Command Line Interface (AWS
-- CLI). For sensitive values, we recommend you use an environment variable
-- of type @PARAMETER_STORE@ or @SECRETS_MANAGER@.
environmentVariable_value :: Lens.Lens' EnvironmentVariable Prelude.Text
environmentVariable_value = Lens.lens (\EnvironmentVariable' {value} -> value) (\s@EnvironmentVariable' {} a -> s {value = a} :: EnvironmentVariable)

instance Prelude.FromJSON EnvironmentVariable where
  parseJSON =
    Prelude.withObject
      "EnvironmentVariable"
      ( \x ->
          EnvironmentVariable'
            Prelude.<$> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "value")
      )

instance Prelude.Hashable EnvironmentVariable

instance Prelude.NFData EnvironmentVariable

instance Prelude.ToJSON EnvironmentVariable where
  toJSON EnvironmentVariable' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("type" Prelude..=) Prelude.<$> type',
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("value" Prelude..= value)
          ]
      )
