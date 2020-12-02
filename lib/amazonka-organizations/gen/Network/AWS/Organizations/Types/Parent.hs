{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Parent where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.ParentType
import Network.AWS.Prelude

-- | Contains information about either a root or an organizational unit (OU) that can contain OUs or accounts in an organization.
--
--
--
-- /See:/ 'parent' smart constructor.
data Parent = Parent'
  { _pId :: !(Maybe Text),
    _pType :: !(Maybe ParentType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pId' - The unique identifier (ID) of the parent entity. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- * 'pType' - The type of the parent entity.
parent ::
  Parent
parent = Parent' {_pId = Nothing, _pType = Nothing}

-- | The unique identifier (ID) of the parent entity. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
pId :: Lens' Parent (Maybe Text)
pId = lens _pId (\s a -> s {_pId = a})

-- | The type of the parent entity.
pType :: Lens' Parent (Maybe ParentType)
pType = lens _pType (\s a -> s {_pType = a})

instance FromJSON Parent where
  parseJSON =
    withObject
      "Parent"
      (\x -> Parent' <$> (x .:? "Id") <*> (x .:? "Type"))

instance Hashable Parent

instance NFData Parent
