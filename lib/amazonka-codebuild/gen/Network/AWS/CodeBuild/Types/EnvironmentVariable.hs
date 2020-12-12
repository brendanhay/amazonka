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
    evType,
    evName,
    evValue,
  )
where

import Network.AWS.CodeBuild.Types.EnvironmentVariableType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an environment variable for a build project or a build.
--
-- /See:/ 'mkEnvironmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { type' ::
      Lude.Maybe EnvironmentVariableType,
    name :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentVariable' with the minimum fields required to make a request.
--
-- * 'name' - The name or key of the environment variable.
-- * 'type'' - The type of environment variable. Valid values include:
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
-- * 'value' - The value of the environment variable.
--
-- /Important:/ We strongly discourage the use of @PLAINTEXT@ environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. @PLAINTEXT@ environment variables can be displayed in plain text using the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI). For sensitive values, we recommend you use an environment variable of type @PARAMETER_STORE@ or @SECRETS_MANAGER@ .
mkEnvironmentVariable ::
  -- | 'name'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  EnvironmentVariable
mkEnvironmentVariable pName_ pValue_ =
  EnvironmentVariable'
    { type' = Lude.Nothing,
      name = pName_,
      value = pValue_
    }

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
evType :: Lens.Lens' EnvironmentVariable (Lude.Maybe EnvironmentVariableType)
evType = Lens.lens (type' :: EnvironmentVariable -> Lude.Maybe EnvironmentVariableType) (\s a -> s {type' = a} :: EnvironmentVariable)
{-# DEPRECATED evType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name or key of the environment variable.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evName :: Lens.Lens' EnvironmentVariable Lude.Text
evName = Lens.lens (name :: EnvironmentVariable -> Lude.Text) (\s a -> s {name = a} :: EnvironmentVariable)
{-# DEPRECATED evName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the environment variable.
--
-- /Important:/ We strongly discourage the use of @PLAINTEXT@ environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. @PLAINTEXT@ environment variables can be displayed in plain text using the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI). For sensitive values, we recommend you use an environment variable of type @PARAMETER_STORE@ or @SECRETS_MANAGER@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evValue :: Lens.Lens' EnvironmentVariable Lude.Text
evValue = Lens.lens (value :: EnvironmentVariable -> Lude.Text) (\s a -> s {value = a} :: EnvironmentVariable)
{-# DEPRECATED evValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON EnvironmentVariable where
  parseJSON =
    Lude.withObject
      "EnvironmentVariable"
      ( \x ->
          EnvironmentVariable'
            Lude.<$> (x Lude..:? "type")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "value")
      )

instance Lude.ToJSON EnvironmentVariable where
  toJSON EnvironmentVariable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("type" Lude..=) Lude.<$> type',
            Lude.Just ("name" Lude..= name),
            Lude.Just ("value" Lude..= value)
          ]
      )
