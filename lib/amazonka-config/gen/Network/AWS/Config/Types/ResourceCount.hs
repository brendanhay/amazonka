{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCount where

import Network.AWS.Config.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains the resource type and the number of resources.
--
--
--
-- /See:/ 'resourceCount' smart constructor.
data ResourceCount = ResourceCount'
  { _resResourceType ::
      !(Maybe ResourceType),
    _resCount :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'resResourceType' - The resource type (for example, @"AWS::EC2::Instance"@ ).
--
-- * 'resCount' - The number of resources.
resourceCount ::
  ResourceCount
resourceCount =
  ResourceCount' {_resResourceType = Nothing, _resCount = Nothing}

-- | The resource type (for example, @"AWS::EC2::Instance"@ ).
resResourceType :: Lens' ResourceCount (Maybe ResourceType)
resResourceType = lens _resResourceType (\s a -> s {_resResourceType = a})

-- | The number of resources.
resCount :: Lens' ResourceCount (Maybe Integer)
resCount = lens _resCount (\s a -> s {_resCount = a})

instance FromJSON ResourceCount where
  parseJSON =
    withObject
      "ResourceCount"
      ( \x ->
          ResourceCount' <$> (x .:? "resourceType") <*> (x .:? "count")
      )

instance Hashable ResourceCount

instance NFData ResourceCount
