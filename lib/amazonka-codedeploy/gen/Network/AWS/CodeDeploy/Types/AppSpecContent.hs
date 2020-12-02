{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AppSpecContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AppSpecContent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A revision for an AWS Lambda or Amazon ECS deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda and Amazon ECS deployments, the revision is the same as the AppSpec file. This method replaces the deprecated @RawString@ data type.
--
--
--
-- /See:/ 'appSpecContent' smart constructor.
data AppSpecContent = AppSpecContent'
  { _ascContent :: !(Maybe Text),
    _ascSha256 :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppSpecContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ascContent' - The YAML-formatted or JSON-formatted revision string.  For an AWS Lambda deployment, the content includes a Lambda function name, the alias for its original version, and the alias for its replacement version. The deployment shifts traffic from the original version of the Lambda function to the replacement version.  For an Amazon ECS deployment, the content includes the task name, information about the load balancer that serves traffic to the container, and more.  For both types of deployments, the content can specify Lambda functions that run at specified hooks, such as @BeforeInstall@ , during a deployment.
--
-- * 'ascSha256' - The SHA256 hash value of the revision content.
appSpecContent ::
  AppSpecContent
appSpecContent =
  AppSpecContent' {_ascContent = Nothing, _ascSha256 = Nothing}

-- | The YAML-formatted or JSON-formatted revision string.  For an AWS Lambda deployment, the content includes a Lambda function name, the alias for its original version, and the alias for its replacement version. The deployment shifts traffic from the original version of the Lambda function to the replacement version.  For an Amazon ECS deployment, the content includes the task name, information about the load balancer that serves traffic to the container, and more.  For both types of deployments, the content can specify Lambda functions that run at specified hooks, such as @BeforeInstall@ , during a deployment.
ascContent :: Lens' AppSpecContent (Maybe Text)
ascContent = lens _ascContent (\s a -> s {_ascContent = a})

-- | The SHA256 hash value of the revision content.
ascSha256 :: Lens' AppSpecContent (Maybe Text)
ascSha256 = lens _ascSha256 (\s a -> s {_ascSha256 = a})

instance FromJSON AppSpecContent where
  parseJSON =
    withObject
      "AppSpecContent"
      (\x -> AppSpecContent' <$> (x .:? "content") <*> (x .:? "sha256"))

instance Hashable AppSpecContent

instance NFData AppSpecContent

instance ToJSON AppSpecContent where
  toJSON AppSpecContent' {..} =
    object
      ( catMaybes
          [("content" .=) <$> _ascContent, ("sha256" .=) <$> _ascSha256]
      )
