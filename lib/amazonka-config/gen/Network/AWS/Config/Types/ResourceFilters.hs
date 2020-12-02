{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceFilters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters the results by resource account ID, region, resource ID, and resource name.
--
--
--
-- /See:/ 'resourceFilters' smart constructor.
data ResourceFilters = ResourceFilters'
  { _rfResourceId ::
      !(Maybe Text),
    _rfResourceName :: !(Maybe Text),
    _rfAccountId :: !(Maybe Text),
    _rfRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfResourceId' - The ID of the resource.
--
-- * 'rfResourceName' - The name of the resource.
--
-- * 'rfAccountId' - The 12-digit source account ID.
--
-- * 'rfRegion' - The source region.
resourceFilters ::
  ResourceFilters
resourceFilters =
  ResourceFilters'
    { _rfResourceId = Nothing,
      _rfResourceName = Nothing,
      _rfAccountId = Nothing,
      _rfRegion = Nothing
    }

-- | The ID of the resource.
rfResourceId :: Lens' ResourceFilters (Maybe Text)
rfResourceId = lens _rfResourceId (\s a -> s {_rfResourceId = a})

-- | The name of the resource.
rfResourceName :: Lens' ResourceFilters (Maybe Text)
rfResourceName = lens _rfResourceName (\s a -> s {_rfResourceName = a})

-- | The 12-digit source account ID.
rfAccountId :: Lens' ResourceFilters (Maybe Text)
rfAccountId = lens _rfAccountId (\s a -> s {_rfAccountId = a})

-- | The source region.
rfRegion :: Lens' ResourceFilters (Maybe Text)
rfRegion = lens _rfRegion (\s a -> s {_rfRegion = a})

instance Hashable ResourceFilters

instance NFData ResourceFilters

instance ToJSON ResourceFilters where
  toJSON ResourceFilters' {..} =
    object
      ( catMaybes
          [ ("ResourceId" .=) <$> _rfResourceId,
            ("ResourceName" .=) <$> _rfResourceName,
            ("AccountId" .=) <$> _rfAccountId,
            ("Region" .=) <$> _rfRegion
          ]
      )
