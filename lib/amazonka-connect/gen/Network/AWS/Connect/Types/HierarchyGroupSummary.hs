{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyGroupSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about a hierarchy group.
--
--
--
-- /See:/ 'hierarchyGroupSummary' smart constructor.
data HierarchyGroupSummary = HierarchyGroupSummary'
  { _hgsARN ::
      !(Maybe Text),
    _hgsName :: !(Maybe Text),
    _hgsId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HierarchyGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hgsARN' - The Amazon Resource Name (ARN) of the hierarchy group.
--
-- * 'hgsName' - The name of the hierarchy group.
--
-- * 'hgsId' - The identifier of the hierarchy group.
hierarchyGroupSummary ::
  HierarchyGroupSummary
hierarchyGroupSummary =
  HierarchyGroupSummary'
    { _hgsARN = Nothing,
      _hgsName = Nothing,
      _hgsId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy group.
hgsARN :: Lens' HierarchyGroupSummary (Maybe Text)
hgsARN = lens _hgsARN (\s a -> s {_hgsARN = a})

-- | The name of the hierarchy group.
hgsName :: Lens' HierarchyGroupSummary (Maybe Text)
hgsName = lens _hgsName (\s a -> s {_hgsName = a})

-- | The identifier of the hierarchy group.
hgsId :: Lens' HierarchyGroupSummary (Maybe Text)
hgsId = lens _hgsId (\s a -> s {_hgsId = a})

instance FromJSON HierarchyGroupSummary where
  parseJSON =
    withObject
      "HierarchyGroupSummary"
      ( \x ->
          HierarchyGroupSummary'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id")
      )

instance Hashable HierarchyGroupSummary

instance NFData HierarchyGroupSummary
