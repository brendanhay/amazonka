{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceAuth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceAuth where

import Network.AWS.CodeBuild.Types.SourceAuthType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built.
--
--
-- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
--
--
-- /See:/ 'sourceAuth' smart constructor.
data SourceAuth = SourceAuth'
  { _saResource :: !(Maybe Text),
    _saType :: !SourceAuthType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceAuth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saResource' - The resource value that applies to the specified authorization type.
--
-- * 'saType' - The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
sourceAuth ::
  -- | 'saType'
  SourceAuthType ->
  SourceAuth
sourceAuth pType_ =
  SourceAuth' {_saResource = Nothing, _saType = pType_}

-- | The resource value that applies to the specified authorization type.
saResource :: Lens' SourceAuth (Maybe Text)
saResource = lens _saResource (\s a -> s {_saResource = a})

-- | The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
saType :: Lens' SourceAuth SourceAuthType
saType = lens _saType (\s a -> s {_saType = a})

instance FromJSON SourceAuth where
  parseJSON =
    withObject
      "SourceAuth"
      (\x -> SourceAuth' <$> (x .:? "resource") <*> (x .: "type"))

instance Hashable SourceAuth

instance NFData SourceAuth

instance ToJSON SourceAuth where
  toJSON SourceAuth' {..} =
    object
      ( catMaybes
          [("resource" .=) <$> _saResource, Just ("type" .= _saType)]
      )
