{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeLogConfigs where

import Network.AWS.CloudFront.Types.RealtimeLogConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of real-time log configurations.
--
-- /See:/ 'newRealtimeLogConfigs' smart constructor.
data RealtimeLogConfigs = RealtimeLogConfigs'
  { -- | Contains the list of real-time log configurations.
    items :: Prelude.Maybe [RealtimeLogConfig],
    -- | If there are more items in the list than are in this response, this
    -- element is present. It contains the value that you should use in the
    -- @Marker@ field of a subsequent request to continue listing real-time log
    -- configurations where you left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of real-time log configurations requested.
    maxItems :: Prelude.Int,
    -- | A flag that indicates whether there are more real-time log
    -- configurations than are contained in this list.
    isTruncated :: Prelude.Bool,
    -- | This parameter indicates where this list of real-time log configurations
    -- begins. This list includes real-time log configurations that occur after
    -- the marker.
    marker :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RealtimeLogConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'realtimeLogConfigs_items' - Contains the list of real-time log configurations.
--
-- 'nextMarker', 'realtimeLogConfigs_nextMarker' - If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing real-time log
-- configurations where you left off.
--
-- 'maxItems', 'realtimeLogConfigs_maxItems' - The maximum number of real-time log configurations requested.
--
-- 'isTruncated', 'realtimeLogConfigs_isTruncated' - A flag that indicates whether there are more real-time log
-- configurations than are contained in this list.
--
-- 'marker', 'realtimeLogConfigs_marker' - This parameter indicates where this list of real-time log configurations
-- begins. This list includes real-time log configurations that occur after
-- the marker.
newRealtimeLogConfigs ::
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'marker'
  Prelude.Text ->
  RealtimeLogConfigs
newRealtimeLogConfigs
  pMaxItems_
  pIsTruncated_
  pMarker_ =
    RealtimeLogConfigs'
      { items = Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        marker = pMarker_
      }

-- | Contains the list of real-time log configurations.
realtimeLogConfigs_items :: Lens.Lens' RealtimeLogConfigs (Prelude.Maybe [RealtimeLogConfig])
realtimeLogConfigs_items = Lens.lens (\RealtimeLogConfigs' {items} -> items) (\s@RealtimeLogConfigs' {} a -> s {items = a} :: RealtimeLogConfigs) Prelude.. Lens.mapping Prelude._Coerce

-- | If there are more items in the list than are in this response, this
-- element is present. It contains the value that you should use in the
-- @Marker@ field of a subsequent request to continue listing real-time log
-- configurations where you left off.
realtimeLogConfigs_nextMarker :: Lens.Lens' RealtimeLogConfigs (Prelude.Maybe Prelude.Text)
realtimeLogConfigs_nextMarker = Lens.lens (\RealtimeLogConfigs' {nextMarker} -> nextMarker) (\s@RealtimeLogConfigs' {} a -> s {nextMarker = a} :: RealtimeLogConfigs)

-- | The maximum number of real-time log configurations requested.
realtimeLogConfigs_maxItems :: Lens.Lens' RealtimeLogConfigs Prelude.Int
realtimeLogConfigs_maxItems = Lens.lens (\RealtimeLogConfigs' {maxItems} -> maxItems) (\s@RealtimeLogConfigs' {} a -> s {maxItems = a} :: RealtimeLogConfigs)

-- | A flag that indicates whether there are more real-time log
-- configurations than are contained in this list.
realtimeLogConfigs_isTruncated :: Lens.Lens' RealtimeLogConfigs Prelude.Bool
realtimeLogConfigs_isTruncated = Lens.lens (\RealtimeLogConfigs' {isTruncated} -> isTruncated) (\s@RealtimeLogConfigs' {} a -> s {isTruncated = a} :: RealtimeLogConfigs)

-- | This parameter indicates where this list of real-time log configurations
-- begins. This list includes real-time log configurations that occur after
-- the marker.
realtimeLogConfigs_marker :: Lens.Lens' RealtimeLogConfigs Prelude.Text
realtimeLogConfigs_marker = Lens.lens (\RealtimeLogConfigs' {marker} -> marker) (\s@RealtimeLogConfigs' {} a -> s {marker = a} :: RealtimeLogConfigs)

instance Prelude.FromXML RealtimeLogConfigs where
  parseXML x =
    RealtimeLogConfigs'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "IsTruncated")
      Prelude.<*> (x Prelude..@ "Marker")

instance Prelude.Hashable RealtimeLogConfigs

instance Prelude.NFData RealtimeLogConfigs
