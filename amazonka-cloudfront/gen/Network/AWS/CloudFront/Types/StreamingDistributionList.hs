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
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionList where

import Network.AWS.CloudFront.Types.StreamingDistributionSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A streaming distribution list.
--
-- /See:/ 'newStreamingDistributionList' smart constructor.
data StreamingDistributionList = StreamingDistributionList'
  { -- | A complex type that contains one @StreamingDistributionSummary@ element
    -- for each distribution that was created by the current AWS account.
    items :: Prelude.Maybe [StreamingDistributionSummary],
    -- | If @IsTruncated@ is @true@, this element is present and contains the
    -- value you can use for the @Marker@ request parameter to continue listing
    -- your RTMP distributions where they left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The value you provided for the @Marker@ request parameter.
    marker :: Prelude.Text,
    -- | The value you provided for the @MaxItems@ request parameter.
    maxItems :: Prelude.Int,
    -- | A flag that indicates whether more streaming distributions remain to be
    -- listed. If your results were truncated, you can make a follow-up
    -- pagination request using the @Marker@ request parameter to retrieve more
    -- distributions in the list.
    isTruncated :: Prelude.Bool,
    -- | The number of streaming distributions that were created by the current
    -- AWS account.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StreamingDistributionList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'streamingDistributionList_items' - A complex type that contains one @StreamingDistributionSummary@ element
-- for each distribution that was created by the current AWS account.
--
-- 'nextMarker', 'streamingDistributionList_nextMarker' - If @IsTruncated@ is @true@, this element is present and contains the
-- value you can use for the @Marker@ request parameter to continue listing
-- your RTMP distributions where they left off.
--
-- 'marker', 'streamingDistributionList_marker' - The value you provided for the @Marker@ request parameter.
--
-- 'maxItems', 'streamingDistributionList_maxItems' - The value you provided for the @MaxItems@ request parameter.
--
-- 'isTruncated', 'streamingDistributionList_isTruncated' - A flag that indicates whether more streaming distributions remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the @Marker@ request parameter to retrieve more
-- distributions in the list.
--
-- 'quantity', 'streamingDistributionList_quantity' - The number of streaming distributions that were created by the current
-- AWS account.
newStreamingDistributionList ::
  -- | 'marker'
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  StreamingDistributionList
newStreamingDistributionList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    StreamingDistributionList'
      { items = Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | A complex type that contains one @StreamingDistributionSummary@ element
-- for each distribution that was created by the current AWS account.
streamingDistributionList_items :: Lens.Lens' StreamingDistributionList (Prelude.Maybe [StreamingDistributionSummary])
streamingDistributionList_items = Lens.lens (\StreamingDistributionList' {items} -> items) (\s@StreamingDistributionList' {} a -> s {items = a} :: StreamingDistributionList) Prelude.. Lens.mapping Prelude._Coerce

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value you can use for the @Marker@ request parameter to continue listing
-- your RTMP distributions where they left off.
streamingDistributionList_nextMarker :: Lens.Lens' StreamingDistributionList (Prelude.Maybe Prelude.Text)
streamingDistributionList_nextMarker = Lens.lens (\StreamingDistributionList' {nextMarker} -> nextMarker) (\s@StreamingDistributionList' {} a -> s {nextMarker = a} :: StreamingDistributionList)

-- | The value you provided for the @Marker@ request parameter.
streamingDistributionList_marker :: Lens.Lens' StreamingDistributionList Prelude.Text
streamingDistributionList_marker = Lens.lens (\StreamingDistributionList' {marker} -> marker) (\s@StreamingDistributionList' {} a -> s {marker = a} :: StreamingDistributionList)

-- | The value you provided for the @MaxItems@ request parameter.
streamingDistributionList_maxItems :: Lens.Lens' StreamingDistributionList Prelude.Int
streamingDistributionList_maxItems = Lens.lens (\StreamingDistributionList' {maxItems} -> maxItems) (\s@StreamingDistributionList' {} a -> s {maxItems = a} :: StreamingDistributionList)

-- | A flag that indicates whether more streaming distributions remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the @Marker@ request parameter to retrieve more
-- distributions in the list.
streamingDistributionList_isTruncated :: Lens.Lens' StreamingDistributionList Prelude.Bool
streamingDistributionList_isTruncated = Lens.lens (\StreamingDistributionList' {isTruncated} -> isTruncated) (\s@StreamingDistributionList' {} a -> s {isTruncated = a} :: StreamingDistributionList)

-- | The number of streaming distributions that were created by the current
-- AWS account.
streamingDistributionList_quantity :: Lens.Lens' StreamingDistributionList Prelude.Int
streamingDistributionList_quantity = Lens.lens (\StreamingDistributionList' {quantity} -> quantity) (\s@StreamingDistributionList' {} a -> s {quantity = a} :: StreamingDistributionList)

instance Prelude.FromXML StreamingDistributionList where
  parseXML x =
    StreamingDistributionList'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        ( Prelude.parseXMLList
                            "StreamingDistributionSummary"
                        )
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "Marker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "IsTruncated")
      Prelude.<*> (x Prelude..@ "Quantity")

instance Prelude.Hashable StreamingDistributionList

instance Prelude.NFData StreamingDistributionList
