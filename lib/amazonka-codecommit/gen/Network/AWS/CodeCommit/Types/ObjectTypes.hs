{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ObjectTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ObjectTypes where

import Network.AWS.CodeCommit.Types.ObjectTypeEnum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the type of an object in a merge operation.
--
--
--
-- /See:/ 'objectTypes' smart constructor.
data ObjectTypes = ObjectTypes'
  { _otDestination ::
      !(Maybe ObjectTypeEnum),
    _otBase :: !(Maybe ObjectTypeEnum),
    _otSource :: !(Maybe ObjectTypeEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ObjectTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'otDestination' - The type of the object in the destination branch.
--
-- * 'otBase' - The type of the object in the base commit of the merge.
--
-- * 'otSource' - The type of the object in the source branch.
objectTypes ::
  ObjectTypes
objectTypes =
  ObjectTypes'
    { _otDestination = Nothing,
      _otBase = Nothing,
      _otSource = Nothing
    }

-- | The type of the object in the destination branch.
otDestination :: Lens' ObjectTypes (Maybe ObjectTypeEnum)
otDestination = lens _otDestination (\s a -> s {_otDestination = a})

-- | The type of the object in the base commit of the merge.
otBase :: Lens' ObjectTypes (Maybe ObjectTypeEnum)
otBase = lens _otBase (\s a -> s {_otBase = a})

-- | The type of the object in the source branch.
otSource :: Lens' ObjectTypes (Maybe ObjectTypeEnum)
otSource = lens _otSource (\s a -> s {_otSource = a})

instance FromJSON ObjectTypes where
  parseJSON =
    withObject
      "ObjectTypes"
      ( \x ->
          ObjectTypes'
            <$> (x .:? "destination") <*> (x .:? "base") <*> (x .:? "source")
      )

instance Hashable ObjectTypes

instance NFData ObjectTypes
