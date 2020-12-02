{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.CodeSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.CodeSource where

import Network.AWS.CodeStar.Types.S3Location
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
--
--
-- /See:/ 'codeSource' smart constructor.
newtype CodeSource = CodeSource' {_csS3 :: S3Location}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csS3' - Information about the Amazon S3 location where the source code files provided with the project request are stored.
codeSource ::
  -- | 'csS3'
  S3Location ->
  CodeSource
codeSource pS3_ = CodeSource' {_csS3 = pS3_}

-- | Information about the Amazon S3 location where the source code files provided with the project request are stored.
csS3 :: Lens' CodeSource S3Location
csS3 = lens _csS3 (\s a -> s {_csS3 = a})

instance Hashable CodeSource

instance NFData CodeSource

instance ToJSON CodeSource where
  toJSON CodeSource' {..} = object (catMaybes [Just ("s3" .= _csS3)])
