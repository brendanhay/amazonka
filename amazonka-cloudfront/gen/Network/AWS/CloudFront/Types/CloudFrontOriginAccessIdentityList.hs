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
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityList where

import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Lists the origin access identities for CloudFront.Send a @GET@ request
-- to the @\/CloudFront API version\/origin-access-identity\/cloudfront@
-- resource. The response includes a @CloudFrontOriginAccessIdentityList@
-- element with zero or more @CloudFrontOriginAccessIdentitySummary@ child
-- elements. By default, your entire list of origin access identities is
-- returned in one single page. If the list is long, you can paginate it
-- using the @MaxItems@ and @Marker@ parameters.
--
-- /See:/ 'newCloudFrontOriginAccessIdentityList' smart constructor.
data CloudFrontOriginAccessIdentityList = CloudFrontOriginAccessIdentityList'
  { -- | A complex type that contains one @CloudFrontOriginAccessIdentitySummary@
    -- element for each origin access identity that was created by the current
    -- AWS account.
    items :: Prelude.Maybe [CloudFrontOriginAccessIdentitySummary],
    -- | If @IsTruncated@ is @true@, this element is present and contains the
    -- value you can use for the @Marker@ request parameter to continue listing
    -- your origin access identities where they left off.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Use this when paginating results to indicate where to begin in your list
    -- of origin access identities. The results include identities in the list
    -- that occur after the marker. To get the next page of results, set the
    -- @Marker@ to the value of the @NextMarker@ from the current page\'s
    -- response (which is also the ID of the last identity on that page).
    marker :: Prelude.Text,
    -- | The maximum number of origin access identities you want in the response
    -- body.
    maxItems :: Prelude.Int,
    -- | A flag that indicates whether more origin access identities remain to be
    -- listed. If your results were truncated, you can make a follow-up
    -- pagination request using the @Marker@ request parameter to retrieve more
    -- items in the list.
    isTruncated :: Prelude.Bool,
    -- | The number of CloudFront origin access identities that were created by
    -- the current AWS account.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CloudFrontOriginAccessIdentityList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'cloudFrontOriginAccessIdentityList_items' - A complex type that contains one @CloudFrontOriginAccessIdentitySummary@
-- element for each origin access identity that was created by the current
-- AWS account.
--
-- 'nextMarker', 'cloudFrontOriginAccessIdentityList_nextMarker' - If @IsTruncated@ is @true@, this element is present and contains the
-- value you can use for the @Marker@ request parameter to continue listing
-- your origin access identities where they left off.
--
-- 'marker', 'cloudFrontOriginAccessIdentityList_marker' - Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last identity on that page).
--
-- 'maxItems', 'cloudFrontOriginAccessIdentityList_maxItems' - The maximum number of origin access identities you want in the response
-- body.
--
-- 'isTruncated', 'cloudFrontOriginAccessIdentityList_isTruncated' - A flag that indicates whether more origin access identities remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the @Marker@ request parameter to retrieve more
-- items in the list.
--
-- 'quantity', 'cloudFrontOriginAccessIdentityList_quantity' - The number of CloudFront origin access identities that were created by
-- the current AWS account.
newCloudFrontOriginAccessIdentityList ::
  -- | 'marker'
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  CloudFrontOriginAccessIdentityList
newCloudFrontOriginAccessIdentityList
  pMarker_
  pMaxItems_
  pIsTruncated_
  pQuantity_ =
    CloudFrontOriginAccessIdentityList'
      { items =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        marker = pMarker_,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        quantity = pQuantity_
      }

-- | A complex type that contains one @CloudFrontOriginAccessIdentitySummary@
-- element for each origin access identity that was created by the current
-- AWS account.
cloudFrontOriginAccessIdentityList_items :: Lens.Lens' CloudFrontOriginAccessIdentityList (Prelude.Maybe [CloudFrontOriginAccessIdentitySummary])
cloudFrontOriginAccessIdentityList_items = Lens.lens (\CloudFrontOriginAccessIdentityList' {items} -> items) (\s@CloudFrontOriginAccessIdentityList' {} a -> s {items = a} :: CloudFrontOriginAccessIdentityList) Prelude.. Lens.mapping Prelude._Coerce

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value you can use for the @Marker@ request parameter to continue listing
-- your origin access identities where they left off.
cloudFrontOriginAccessIdentityList_nextMarker :: Lens.Lens' CloudFrontOriginAccessIdentityList (Prelude.Maybe Prelude.Text)
cloudFrontOriginAccessIdentityList_nextMarker = Lens.lens (\CloudFrontOriginAccessIdentityList' {nextMarker} -> nextMarker) (\s@CloudFrontOriginAccessIdentityList' {} a -> s {nextMarker = a} :: CloudFrontOriginAccessIdentityList)

-- | Use this when paginating results to indicate where to begin in your list
-- of origin access identities. The results include identities in the list
-- that occur after the marker. To get the next page of results, set the
-- @Marker@ to the value of the @NextMarker@ from the current page\'s
-- response (which is also the ID of the last identity on that page).
cloudFrontOriginAccessIdentityList_marker :: Lens.Lens' CloudFrontOriginAccessIdentityList Prelude.Text
cloudFrontOriginAccessIdentityList_marker = Lens.lens (\CloudFrontOriginAccessIdentityList' {marker} -> marker) (\s@CloudFrontOriginAccessIdentityList' {} a -> s {marker = a} :: CloudFrontOriginAccessIdentityList)

-- | The maximum number of origin access identities you want in the response
-- body.
cloudFrontOriginAccessIdentityList_maxItems :: Lens.Lens' CloudFrontOriginAccessIdentityList Prelude.Int
cloudFrontOriginAccessIdentityList_maxItems = Lens.lens (\CloudFrontOriginAccessIdentityList' {maxItems} -> maxItems) (\s@CloudFrontOriginAccessIdentityList' {} a -> s {maxItems = a} :: CloudFrontOriginAccessIdentityList)

-- | A flag that indicates whether more origin access identities remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request using the @Marker@ request parameter to retrieve more
-- items in the list.
cloudFrontOriginAccessIdentityList_isTruncated :: Lens.Lens' CloudFrontOriginAccessIdentityList Prelude.Bool
cloudFrontOriginAccessIdentityList_isTruncated = Lens.lens (\CloudFrontOriginAccessIdentityList' {isTruncated} -> isTruncated) (\s@CloudFrontOriginAccessIdentityList' {} a -> s {isTruncated = a} :: CloudFrontOriginAccessIdentityList)

-- | The number of CloudFront origin access identities that were created by
-- the current AWS account.
cloudFrontOriginAccessIdentityList_quantity :: Lens.Lens' CloudFrontOriginAccessIdentityList Prelude.Int
cloudFrontOriginAccessIdentityList_quantity = Lens.lens (\CloudFrontOriginAccessIdentityList' {quantity} -> quantity) (\s@CloudFrontOriginAccessIdentityList' {} a -> s {quantity = a} :: CloudFrontOriginAccessIdentityList)

instance
  Prelude.FromXML
    CloudFrontOriginAccessIdentityList
  where
  parseXML x =
    CloudFrontOriginAccessIdentityList'
      Prelude.<$> ( x Prelude..@? "Items" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        ( Prelude.parseXMLList
                            "CloudFrontOriginAccessIdentitySummary"
                        )
                  )
      Prelude.<*> (x Prelude..@? "NextMarker")
      Prelude.<*> (x Prelude..@ "Marker")
      Prelude.<*> (x Prelude..@ "MaxItems")
      Prelude.<*> (x Prelude..@ "IsTruncated")
      Prelude.<*> (x Prelude..@ "Quantity")

instance
  Prelude.Hashable
    CloudFrontOriginAccessIdentityList

instance
  Prelude.NFData
    CloudFrontOriginAccessIdentityList
