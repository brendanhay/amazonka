{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AddThingsToThingGroupParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AddThingsToThingGroupParams where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters used when defining a mitigation action that move a set of things to a thing group.
--
--
--
-- /See:/ 'addThingsToThingGroupParams' smart constructor.
data AddThingsToThingGroupParams = AddThingsToThingGroupParams'
  { _atttgpOverrideDynamicGroups ::
      !(Maybe Bool),
    _atttgpThingGroupNames ::
      !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddThingsToThingGroupParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atttgpOverrideDynamicGroups' - Specifies if this mitigation action can move the things that triggered the mitigation action even if they are part of one or more dynamic things groups.
--
-- * 'atttgpThingGroupNames' - The list of groups to which you want to add the things that triggered the mitigation action. You can add a thing to a maximum of 10 groups, but you cannot add a thing to more than one group in the same hierarchy.
addThingsToThingGroupParams ::
  -- | 'atttgpThingGroupNames'
  NonEmpty Text ->
  AddThingsToThingGroupParams
addThingsToThingGroupParams pThingGroupNames_ =
  AddThingsToThingGroupParams'
    { _atttgpOverrideDynamicGroups =
        Nothing,
      _atttgpThingGroupNames = _List1 # pThingGroupNames_
    }

-- | Specifies if this mitigation action can move the things that triggered the mitigation action even if they are part of one or more dynamic things groups.
atttgpOverrideDynamicGroups :: Lens' AddThingsToThingGroupParams (Maybe Bool)
atttgpOverrideDynamicGroups = lens _atttgpOverrideDynamicGroups (\s a -> s {_atttgpOverrideDynamicGroups = a})

-- | The list of groups to which you want to add the things that triggered the mitigation action. You can add a thing to a maximum of 10 groups, but you cannot add a thing to more than one group in the same hierarchy.
atttgpThingGroupNames :: Lens' AddThingsToThingGroupParams (NonEmpty Text)
atttgpThingGroupNames = lens _atttgpThingGroupNames (\s a -> s {_atttgpThingGroupNames = a}) . _List1

instance FromJSON AddThingsToThingGroupParams where
  parseJSON =
    withObject
      "AddThingsToThingGroupParams"
      ( \x ->
          AddThingsToThingGroupParams'
            <$> (x .:? "overrideDynamicGroups") <*> (x .: "thingGroupNames")
      )

instance Hashable AddThingsToThingGroupParams

instance NFData AddThingsToThingGroupParams

instance ToJSON AddThingsToThingGroupParams where
  toJSON AddThingsToThingGroupParams' {..} =
    object
      ( catMaybes
          [ ("overrideDynamicGroups" .=) <$> _atttgpOverrideDynamicGroups,
            Just ("thingGroupNames" .= _atttgpThingGroupNames)
          ]
      )
