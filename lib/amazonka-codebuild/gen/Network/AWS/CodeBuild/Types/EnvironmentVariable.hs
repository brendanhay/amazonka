{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentVariable
  ( EnvironmentVariable (..),

    -- * Smart constructor
    mkEnvironmentVariable,

    -- * Lenses
    evName,
    evValue,
    evType,
  )
where

import qualified Network.AWS.CodeBuild.Types.EnvironmentVariableType as Types
import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an environment variable for a build project or a build.
--
-- /See:/ 'mkEnvironmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { -- | The name or key of the environment variable.
    name :: Types.NonEmptyString,
    -- | The value of the environment variable.
    --
    -- /Important:/ We strongly discourage the use of @PLAINTEXT@ environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. @PLAINTEXT@ environment variables can be displayed in plain text using the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI). For sensitive values, we recommend you use an environment variable of type @PARAMETER_STORE@ or @SECRETS_MANAGER@ .
    value :: Types.String,
    -- | The type of environment variable. Valid values include:
    --
    --
    --     * @PARAMETER_STORE@ : An environment variable stored in Amazon EC2 Systems Manager Parameter Store. To learn how to specify a parameter store environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.parameter-store env/parameter-store> in the /AWS CodeBuild User Guide/ .
    --
    --
    --     * @PLAINTEXT@ : An environment variable in plain text format. This is the default value.
    --
    --
    --     * @SECRETS_MANAGER@ : An environment variable stored in AWS Secrets Manager. To learn how to specify a secrets manager environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.secrets-manager env/secrets-manager> in the /AWS CodeBuild User Guide/ .
    type' :: Core.Maybe Types.EnvironmentVariableType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentVariable' value with any optional fields omitted.
mkEnvironmentVariable ::
  -- | 'name'
  Types.NonEmptyString ->
  -- | 'value'
  Types.String ->
  EnvironmentVariable
mkEnvironmentVariable name value =
  EnvironmentVariable' {name, value, type' = Core.Nothing}

-- | The name or key of the environment variable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evName :: Lens.Lens' EnvironmentVariable Types.NonEmptyString
evName = Lens.field @"name"
{-# DEPRECATED evName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the environment variable.
--
-- /Important:/ We strongly discourage the use of @PLAINTEXT@ environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. @PLAINTEXT@ environment variables can be displayed in plain text using the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI). For sensitive values, we recommend you use an environment variable of type @PARAMETER_STORE@ or @SECRETS_MANAGER@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evValue :: Lens.Lens' EnvironmentVariable Types.String
evValue = Lens.field @"value"
{-# DEPRECATED evValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of environment variable. Valid values include:
--
--
--     * @PARAMETER_STORE@ : An environment variable stored in Amazon EC2 Systems Manager Parameter Store. To learn how to specify a parameter store environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.parameter-store env/parameter-store> in the /AWS CodeBuild User Guide/ .
--
--
--     * @PLAINTEXT@ : An environment variable in plain text format. This is the default value.
--
--
--     * @SECRETS_MANAGER@ : An environment variable stored in AWS Secrets Manager. To learn how to specify a secrets manager environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.secrets-manager env/secrets-manager> in the /AWS CodeBuild User Guide/ .
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evType :: Lens.Lens' EnvironmentVariable (Core.Maybe Types.EnvironmentVariableType)
evType = Lens.field @"type'"
{-# DEPRECATED evType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON EnvironmentVariable where
  toJSON EnvironmentVariable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("value" Core..= value),
            ("type" Core..=) Core.<$> type'
          ]
      )

instance Core.FromJSON EnvironmentVariable where
  parseJSON =
    Core.withObject "EnvironmentVariable" Core.$
      \x ->
        EnvironmentVariable'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..: "value")
          Core.<*> (x Core..:? "type")
