{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateTagSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateTagSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ResourceType
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The tag specification for the launch template.
--
--
--
-- /See:/ 'launchTemplateTagSpecification' smart constructor.
data LaunchTemplateTagSpecification = LaunchTemplateTagSpecification'
  { _lttsResourceType ::
      !(Maybe ResourceType),
    _lttsTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateTagSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lttsResourceType' - The type of resource.
--
-- * 'lttsTags' - The tags for the resource.
launchTemplateTagSpecification ::
  LaunchTemplateTagSpecification
launchTemplateTagSpecification =
  LaunchTemplateTagSpecification'
    { _lttsResourceType = Nothing,
      _lttsTags = Nothing
    }

-- | The type of resource.
lttsResourceType :: Lens' LaunchTemplateTagSpecification (Maybe ResourceType)
lttsResourceType = lens _lttsResourceType (\s a -> s {_lttsResourceType = a})

-- | The tags for the resource.
lttsTags :: Lens' LaunchTemplateTagSpecification [Tag]
lttsTags = lens _lttsTags (\s a -> s {_lttsTags = a}) . _Default . _Coerce

instance FromXML LaunchTemplateTagSpecification where
  parseXML x =
    LaunchTemplateTagSpecification'
      <$> (x .@? "resourceType")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable LaunchTemplateTagSpecification

instance NFData LaunchTemplateTagSpecification
