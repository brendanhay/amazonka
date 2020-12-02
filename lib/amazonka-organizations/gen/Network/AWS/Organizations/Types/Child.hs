{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Child
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Child where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.ChildType
import Network.AWS.Prelude

-- | Contains a list of child entities, either OUs or accounts.
--
--
--
-- /See:/ 'child' smart constructor.
data Child = Child'
  { _cId :: !(Maybe Text),
    _cType :: !(Maybe ChildType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Child' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cId' - The unique identifier (ID) of this child entity. The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:     * __Account__ - A string that consists of exactly 12 digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- * 'cType' - The type of this child entity.
child ::
  Child
child = Child' {_cId = Nothing, _cType = Nothing}

-- | The unique identifier (ID) of this child entity. The <http://wikipedia.org/wiki/regex regex pattern> for a child ID string requires one of the following:     * __Account__ - A string that consists of exactly 12 digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that contains the OU). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
cId :: Lens' Child (Maybe Text)
cId = lens _cId (\s a -> s {_cId = a})

-- | The type of this child entity.
cType :: Lens' Child (Maybe ChildType)
cType = lens _cType (\s a -> s {_cType = a})

instance FromJSON Child where
  parseJSON =
    withObject
      "Child"
      (\x -> Child' <$> (x .:? "Id") <*> (x .:? "Type"))

instance Hashable Child

instance NFData Child
