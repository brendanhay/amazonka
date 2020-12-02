{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyLevel where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a hierarchy level.
--
--
--
-- /See:/ 'hierarchyLevel' smart constructor.
data HierarchyLevel = HierarchyLevel'
  { _hlARN :: !(Maybe Text),
    _hlName :: !(Maybe Text),
    _hlId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HierarchyLevel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlARN' - The Amazon Resource Name (ARN) of the hierarchy level.
--
-- * 'hlName' - The name of the hierarchy level.
--
-- * 'hlId' - The identifier of the hierarchy level.
hierarchyLevel ::
  HierarchyLevel
hierarchyLevel =
  HierarchyLevel'
    { _hlARN = Nothing,
      _hlName = Nothing,
      _hlId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy level.
hlARN :: Lens' HierarchyLevel (Maybe Text)
hlARN = lens _hlARN (\s a -> s {_hlARN = a})

-- | The name of the hierarchy level.
hlName :: Lens' HierarchyLevel (Maybe Text)
hlName = lens _hlName (\s a -> s {_hlName = a})

-- | The identifier of the hierarchy level.
hlId :: Lens' HierarchyLevel (Maybe Text)
hlId = lens _hlId (\s a -> s {_hlId = a})

instance FromJSON HierarchyLevel where
  parseJSON =
    withObject
      "HierarchyLevel"
      ( \x ->
          HierarchyLevel'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id")
      )

instance Hashable HierarchyLevel

instance NFData HierarchyLevel
