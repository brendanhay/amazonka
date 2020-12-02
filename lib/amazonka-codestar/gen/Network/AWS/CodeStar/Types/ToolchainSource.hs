{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ToolchainSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ToolchainSource where

import Network.AWS.CodeStar.Types.S3Location
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
--
--
--
-- /See:/ 'toolchainSource' smart constructor.
newtype ToolchainSource = ToolchainSource' {_tsS3 :: S3Location}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ToolchainSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsS3' - The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
toolchainSource ::
  -- | 'tsS3'
  S3Location ->
  ToolchainSource
toolchainSource pS3_ = ToolchainSource' {_tsS3 = pS3_}

-- | The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
tsS3 :: Lens' ToolchainSource S3Location
tsS3 = lens _tsS3 (\s a -> s {_tsS3 = a})

instance Hashable ToolchainSource

instance NFData ToolchainSource

instance ToJSON ToolchainSource where
  toJSON ToolchainSource' {..} =
    object (catMaybes [Just ("s3" .= _tsS3)])
