{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Toolchain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Toolchain where

import Network.AWS.CodeStar.Types.ToolchainSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The toolchain template file provided with the project request. AWS CodeStar uses the template to provision the toolchain stack in AWS CloudFormation.
--
--
--
-- /See:/ 'toolchain' smart constructor.
data Toolchain = Toolchain'
  { _tStackParameters ::
      !(Maybe (Map Text (Sensitive Text))),
    _tRoleARN :: !(Maybe Text),
    _tSource :: !ToolchainSource
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Toolchain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStackParameters' - The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
--
-- * 'tRoleARN' - The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
--
-- * 'tSource' - The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
toolchain ::
  -- | 'tSource'
  ToolchainSource ->
  Toolchain
toolchain pSource_ =
  Toolchain'
    { _tStackParameters = Nothing,
      _tRoleARN = Nothing,
      _tSource = pSource_
    }

-- | The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
tStackParameters :: Lens' Toolchain (HashMap Text (Text))
tStackParameters = lens _tStackParameters (\s a -> s {_tStackParameters = a}) . _Default . _Map

-- | The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
tRoleARN :: Lens' Toolchain (Maybe Text)
tRoleARN = lens _tRoleARN (\s a -> s {_tRoleARN = a})

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
tSource :: Lens' Toolchain ToolchainSource
tSource = lens _tSource (\s a -> s {_tSource = a})

instance Hashable Toolchain

instance NFData Toolchain

instance ToJSON Toolchain where
  toJSON Toolchain' {..} =
    object
      ( catMaybes
          [ ("stackParameters" .=) <$> _tStackParameters,
            ("roleArn" .=) <$> _tRoleARN,
            Just ("source" .= _tSource)
          ]
      )
