{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.ResourceIdentifier where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The ARN of a resource, and its resource type.
--
--
--
-- /See:/ 'resourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { _riResourceType ::
      !(Maybe Text),
    _riResourceARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riResourceType' - The resource type of a resource, such as @AWS::EC2::Instance@ .
--
-- * 'riResourceARN' - The ARN of a resource.
resourceIdentifier ::
  ResourceIdentifier
resourceIdentifier =
  ResourceIdentifier'
    { _riResourceType = Nothing,
      _riResourceARN = Nothing
    }

-- | The resource type of a resource, such as @AWS::EC2::Instance@ .
riResourceType :: Lens' ResourceIdentifier (Maybe Text)
riResourceType = lens _riResourceType (\s a -> s {_riResourceType = a})

-- | The ARN of a resource.
riResourceARN :: Lens' ResourceIdentifier (Maybe Text)
riResourceARN = lens _riResourceARN (\s a -> s {_riResourceARN = a})

instance FromJSON ResourceIdentifier where
  parseJSON =
    withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            <$> (x .:? "ResourceType") <*> (x .:? "ResourceArn")
      )

instance Hashable ResourceIdentifier

instance NFData ResourceIdentifier
