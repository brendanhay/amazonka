{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceCredentialsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceCredentialsInfo where

import Network.AWS.CodeBuild.Types.AuthType
import Network.AWS.CodeBuild.Types.ServerType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the credentials for a GitHub, GitHub Enterprise, or Bitbucket repository.
--
--
--
-- /See:/ 'sourceCredentialsInfo' smart constructor.
data SourceCredentialsInfo = SourceCredentialsInfo'
  { _sciArn ::
      !(Maybe Text),
    _sciServerType :: !(Maybe ServerType),
    _sciAuthType :: !(Maybe AuthType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceCredentialsInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sciArn' - The Amazon Resource Name (ARN) of the token.
--
-- * 'sciServerType' - The type of source provider. The valid options are GITHUB, GITHUB_ENTERPRISE, or BITBUCKET.
--
-- * 'sciAuthType' - The type of authentication used by the credentials. Valid options are OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
sourceCredentialsInfo ::
  SourceCredentialsInfo
sourceCredentialsInfo =
  SourceCredentialsInfo'
    { _sciArn = Nothing,
      _sciServerType = Nothing,
      _sciAuthType = Nothing
    }

-- | The Amazon Resource Name (ARN) of the token.
sciArn :: Lens' SourceCredentialsInfo (Maybe Text)
sciArn = lens _sciArn (\s a -> s {_sciArn = a})

-- | The type of source provider. The valid options are GITHUB, GITHUB_ENTERPRISE, or BITBUCKET.
sciServerType :: Lens' SourceCredentialsInfo (Maybe ServerType)
sciServerType = lens _sciServerType (\s a -> s {_sciServerType = a})

-- | The type of authentication used by the credentials. Valid options are OAUTH, BASIC_AUTH, or PERSONAL_ACCESS_TOKEN.
sciAuthType :: Lens' SourceCredentialsInfo (Maybe AuthType)
sciAuthType = lens _sciAuthType (\s a -> s {_sciAuthType = a})

instance FromJSON SourceCredentialsInfo where
  parseJSON =
    withObject
      "SourceCredentialsInfo"
      ( \x ->
          SourceCredentialsInfo'
            <$> (x .:? "arn") <*> (x .:? "serverType") <*> (x .:? "authType")
      )

instance Hashable SourceCredentialsInfo

instance NFData SourceCredentialsInfo
