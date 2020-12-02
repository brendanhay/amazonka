{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentVariable where

import Network.AWS.CodeBuild.Types.EnvironmentVariableType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an environment variable for a build project or a build.
--
--
--
-- /See:/ 'environmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { _evType ::
      !(Maybe EnvironmentVariableType),
    _evName :: !Text,
    _evValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnvironmentVariable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evType' - The type of environment variable. Valid values include:     * @PARAMETER_STORE@ : An environment variable stored in Amazon EC2 Systems Manager Parameter Store. To learn how to specify a parameter store environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.parameter-store env/parameter-store> in the /AWS CodeBuild User Guide/ .     * @PLAINTEXT@ : An environment variable in plain text format. This is the default value.     * @SECRETS_MANAGER@ : An environment variable stored in AWS Secrets Manager. To learn how to specify a secrets manager environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.secrets-manager env/secrets-manager> in the /AWS CodeBuild User Guide/ .
--
-- * 'evName' - The name or key of the environment variable.
--
-- * 'evValue' - The value of the environment variable. /Important:/ We strongly discourage the use of @PLAINTEXT@ environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. @PLAINTEXT@ environment variables can be displayed in plain text using the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI). For sensitive values, we recommend you use an environment variable of type @PARAMETER_STORE@ or @SECRETS_MANAGER@ .
environmentVariable ::
  -- | 'evName'
  Text ->
  -- | 'evValue'
  Text ->
  EnvironmentVariable
environmentVariable pName_ pValue_ =
  EnvironmentVariable'
    { _evType = Nothing,
      _evName = pName_,
      _evValue = pValue_
    }

-- | The type of environment variable. Valid values include:     * @PARAMETER_STORE@ : An environment variable stored in Amazon EC2 Systems Manager Parameter Store. To learn how to specify a parameter store environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.parameter-store env/parameter-store> in the /AWS CodeBuild User Guide/ .     * @PLAINTEXT@ : An environment variable in plain text format. This is the default value.     * @SECRETS_MANAGER@ : An environment variable stored in AWS Secrets Manager. To learn how to specify a secrets manager environment variable, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec.env.secrets-manager env/secrets-manager> in the /AWS CodeBuild User Guide/ .
evType :: Lens' EnvironmentVariable (Maybe EnvironmentVariableType)
evType = lens _evType (\s a -> s {_evType = a})

-- | The name or key of the environment variable.
evName :: Lens' EnvironmentVariable Text
evName = lens _evName (\s a -> s {_evName = a})

-- | The value of the environment variable. /Important:/ We strongly discourage the use of @PLAINTEXT@ environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. @PLAINTEXT@ environment variables can be displayed in plain text using the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI). For sensitive values, we recommend you use an environment variable of type @PARAMETER_STORE@ or @SECRETS_MANAGER@ .
evValue :: Lens' EnvironmentVariable Text
evValue = lens _evValue (\s a -> s {_evValue = a})

instance FromJSON EnvironmentVariable where
  parseJSON =
    withObject
      "EnvironmentVariable"
      ( \x ->
          EnvironmentVariable'
            <$> (x .:? "type") <*> (x .: "name") <*> (x .: "value")
      )

instance Hashable EnvironmentVariable

instance NFData EnvironmentVariable

instance ToJSON EnvironmentVariable where
  toJSON EnvironmentVariable' {..} =
    object
      ( catMaybes
          [ ("type" .=) <$> _evType,
            Just ("name" .= _evName),
            Just ("value" .= _evValue)
          ]
      )
