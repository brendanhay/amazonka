{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.RequiresRecreation
import Network.AWS.ServiceCatalog.Types.ResourceAttribute

-- | Information about a change to a resource attribute.
--
--
--
-- /See:/ 'resourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { _rtdAttribute ::
      !(Maybe ResourceAttribute),
    _rtdRequiresRecreation ::
      !(Maybe RequiresRecreation),
    _rtdName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceTargetDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtdAttribute' - The attribute to be changed.
--
-- * 'rtdRequiresRecreation' - If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
--
-- * 'rtdName' - If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
resourceTargetDefinition ::
  ResourceTargetDefinition
resourceTargetDefinition =
  ResourceTargetDefinition'
    { _rtdAttribute = Nothing,
      _rtdRequiresRecreation = Nothing,
      _rtdName = Nothing
    }

-- | The attribute to be changed.
rtdAttribute :: Lens' ResourceTargetDefinition (Maybe ResourceAttribute)
rtdAttribute = lens _rtdAttribute (\s a -> s {_rtdAttribute = a})

-- | If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
rtdRequiresRecreation :: Lens' ResourceTargetDefinition (Maybe RequiresRecreation)
rtdRequiresRecreation = lens _rtdRequiresRecreation (\s a -> s {_rtdRequiresRecreation = a})

-- | If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
rtdName :: Lens' ResourceTargetDefinition (Maybe Text)
rtdName = lens _rtdName (\s a -> s {_rtdName = a})

instance FromJSON ResourceTargetDefinition where
  parseJSON =
    withObject
      "ResourceTargetDefinition"
      ( \x ->
          ResourceTargetDefinition'
            <$> (x .:? "Attribute")
            <*> (x .:? "RequiresRecreation")
            <*> (x .:? "Name")
      )

instance Hashable ResourceTargetDefinition

instance NFData ResourceTargetDefinition
