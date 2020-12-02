{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotSortingEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotSortingEntity where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SnapshotAttributeToSortBy
import Network.AWS.Redshift.Types.SortByOrder

-- | Describes a sorting entity
--
--
--
-- /See:/ 'snapshotSortingEntity' smart constructor.
data SnapshotSortingEntity = SnapshotSortingEntity'
  { _sseSortOrder ::
      !(Maybe SortByOrder),
    _sseAttribute :: !SnapshotAttributeToSortBy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnapshotSortingEntity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sseSortOrder' - The order for listing the attributes.
--
-- * 'sseAttribute' - The category for sorting the snapshots.
snapshotSortingEntity ::
  -- | 'sseAttribute'
  SnapshotAttributeToSortBy ->
  SnapshotSortingEntity
snapshotSortingEntity pAttribute_ =
  SnapshotSortingEntity'
    { _sseSortOrder = Nothing,
      _sseAttribute = pAttribute_
    }

-- | The order for listing the attributes.
sseSortOrder :: Lens' SnapshotSortingEntity (Maybe SortByOrder)
sseSortOrder = lens _sseSortOrder (\s a -> s {_sseSortOrder = a})

-- | The category for sorting the snapshots.
sseAttribute :: Lens' SnapshotSortingEntity SnapshotAttributeToSortBy
sseAttribute = lens _sseAttribute (\s a -> s {_sseAttribute = a})

instance Hashable SnapshotSortingEntity

instance NFData SnapshotSortingEntity

instance ToQuery SnapshotSortingEntity where
  toQuery SnapshotSortingEntity' {..} =
    mconcat
      ["SortOrder" =: _sseSortOrder, "Attribute" =: _sseAttribute]
