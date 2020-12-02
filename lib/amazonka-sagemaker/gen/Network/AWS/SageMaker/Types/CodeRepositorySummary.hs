{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CodeRepositorySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CodeRepositorySummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.GitConfig

-- | Specifies summary information about a Git repository.
--
--
--
-- /See:/ 'codeRepositorySummary' smart constructor.
data CodeRepositorySummary = CodeRepositorySummary'
  { _crsGitConfig ::
      !(Maybe GitConfig),
    _crsCodeRepositoryName :: !Text,
    _crsCodeRepositoryARN :: !Text,
    _crsCreationTime :: !POSIX,
    _crsLastModifiedTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeRepositorySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsGitConfig' - Configuration details for the Git repository, including the URL where it is located and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
-- * 'crsCodeRepositoryName' - The name of the Git repository.
--
-- * 'crsCodeRepositoryARN' - The Amazon Resource Name (ARN) of the Git repository.
--
-- * 'crsCreationTime' - The date and time that the Git repository was created.
--
-- * 'crsLastModifiedTime' - The date and time that the Git repository was last modified.
codeRepositorySummary ::
  -- | 'crsCodeRepositoryName'
  Text ->
  -- | 'crsCodeRepositoryARN'
  Text ->
  -- | 'crsCreationTime'
  UTCTime ->
  -- | 'crsLastModifiedTime'
  UTCTime ->
  CodeRepositorySummary
codeRepositorySummary
  pCodeRepositoryName_
  pCodeRepositoryARN_
  pCreationTime_
  pLastModifiedTime_ =
    CodeRepositorySummary'
      { _crsGitConfig = Nothing,
        _crsCodeRepositoryName = pCodeRepositoryName_,
        _crsCodeRepositoryARN = pCodeRepositoryARN_,
        _crsCreationTime = _Time # pCreationTime_,
        _crsLastModifiedTime = _Time # pLastModifiedTime_
      }

-- | Configuration details for the Git repository, including the URL where it is located and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
crsGitConfig :: Lens' CodeRepositorySummary (Maybe GitConfig)
crsGitConfig = lens _crsGitConfig (\s a -> s {_crsGitConfig = a})

-- | The name of the Git repository.
crsCodeRepositoryName :: Lens' CodeRepositorySummary Text
crsCodeRepositoryName = lens _crsCodeRepositoryName (\s a -> s {_crsCodeRepositoryName = a})

-- | The Amazon Resource Name (ARN) of the Git repository.
crsCodeRepositoryARN :: Lens' CodeRepositorySummary Text
crsCodeRepositoryARN = lens _crsCodeRepositoryARN (\s a -> s {_crsCodeRepositoryARN = a})

-- | The date and time that the Git repository was created.
crsCreationTime :: Lens' CodeRepositorySummary UTCTime
crsCreationTime = lens _crsCreationTime (\s a -> s {_crsCreationTime = a}) . _Time

-- | The date and time that the Git repository was last modified.
crsLastModifiedTime :: Lens' CodeRepositorySummary UTCTime
crsLastModifiedTime = lens _crsLastModifiedTime (\s a -> s {_crsLastModifiedTime = a}) . _Time

instance FromJSON CodeRepositorySummary where
  parseJSON =
    withObject
      "CodeRepositorySummary"
      ( \x ->
          CodeRepositorySummary'
            <$> (x .:? "GitConfig")
            <*> (x .: "CodeRepositoryName")
            <*> (x .: "CodeRepositoryArn")
            <*> (x .: "CreationTime")
            <*> (x .: "LastModifiedTime")
      )

instance Hashable CodeRepositorySummary

instance NFData CodeRepositorySummary
