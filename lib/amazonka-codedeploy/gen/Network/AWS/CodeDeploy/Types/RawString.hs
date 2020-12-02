{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RawString
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RawString where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.
--
--
--
-- /See:/ 'rawString' smart constructor.
data RawString = RawString'
  { _rsContent :: !(Maybe Text),
    _rsSha256 :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RawString' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsContent' - The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
--
-- * 'rsSha256' - The SHA256 hash value of the revision content.
rawString ::
  RawString
rawString = RawString' {_rsContent = Nothing, _rsSha256 = Nothing}

-- | The YAML-formatted or JSON-formatted revision string. It includes information about which Lambda function to update and optional Lambda functions that validate deployment lifecycle events.
rsContent :: Lens' RawString (Maybe Text)
rsContent = lens _rsContent (\s a -> s {_rsContent = a})

-- | The SHA256 hash value of the revision content.
rsSha256 :: Lens' RawString (Maybe Text)
rsSha256 = lens _rsSha256 (\s a -> s {_rsSha256 = a})

instance FromJSON RawString where
  parseJSON =
    withObject
      "RawString"
      (\x -> RawString' <$> (x .:? "content") <*> (x .:? "sha256"))

instance Hashable RawString

instance NFData RawString

instance ToJSON RawString where
  toJSON RawString' {..} =
    object
      ( catMaybes
          [("content" .=) <$> _rsContent, ("sha256" .=) <$> _rsSha256]
      )
