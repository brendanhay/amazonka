{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildStatusConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildStatusConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information that defines how the AWS CodeBuild build project reports the build status to the source provider.
--
--
--
-- /See:/ 'buildStatusConfig' smart constructor.
data BuildStatusConfig = BuildStatusConfig'
  { _bscContext ::
      !(Maybe Text),
    _bscTargetURL :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuildStatusConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bscContext' - Specifies the context of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.     * Bitbucket    * This parameter is used for the @name@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.     * GitHub/GitHub Enterprise Server    * This parameter is used for the @context@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
--
-- * 'bscTargetURL' - Specifies the target url of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.     * Bitbucket    * This parameter is used for the @url@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.     * GitHub/GitHub Enterprise Server    * This parameter is used for the @target_url@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
buildStatusConfig ::
  BuildStatusConfig
buildStatusConfig =
  BuildStatusConfig'
    { _bscContext = Nothing,
      _bscTargetURL = Nothing
    }

-- | Specifies the context of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.     * Bitbucket    * This parameter is used for the @name@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.     * GitHub/GitHub Enterprise Server    * This parameter is used for the @context@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
bscContext :: Lens' BuildStatusConfig (Maybe Text)
bscContext = lens _bscContext (\s a -> s {_bscContext = a})

-- | Specifies the target url of the build status CodeBuild sends to the source provider. The usage of this parameter depends on the source provider.     * Bitbucket    * This parameter is used for the @url@ parameter in the Bitbucket commit status. For more information, see <https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories/%7Bworkspace%7D/%7Brepo_slug%7D/commit/%7Bnode%7D/statuses/build build> in the Bitbucket API documentation.     * GitHub/GitHub Enterprise Server    * This parameter is used for the @target_url@ parameter in the GitHub commit status. For more information, see <https://developer.github.com/v3/repos/statuses/#create-a-commit-status Create a commit status> in the GitHub developer guide.
bscTargetURL :: Lens' BuildStatusConfig (Maybe Text)
bscTargetURL = lens _bscTargetURL (\s a -> s {_bscTargetURL = a})

instance FromJSON BuildStatusConfig where
  parseJSON =
    withObject
      "BuildStatusConfig"
      ( \x ->
          BuildStatusConfig' <$> (x .:? "context") <*> (x .:? "targetUrl")
      )

instance Hashable BuildStatusConfig

instance NFData BuildStatusConfig

instance ToJSON BuildStatusConfig where
  toJSON BuildStatusConfig' {..} =
    object
      ( catMaybes
          [ ("context" .=) <$> _bscContext,
            ("targetUrl" .=) <$> _bscTargetURL
          ]
      )
