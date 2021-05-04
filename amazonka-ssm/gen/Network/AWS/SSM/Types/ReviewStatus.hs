{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ReviewStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ReviewStatus
  ( ReviewStatus
      ( ..,
        ReviewStatus_APPROVED,
        ReviewStatus_NOT_REVIEWED,
        ReviewStatus_PENDING,
        ReviewStatus_REJECTED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ReviewStatus = ReviewStatus'
  { fromReviewStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ReviewStatus_APPROVED :: ReviewStatus
pattern ReviewStatus_APPROVED = ReviewStatus' "APPROVED"

pattern ReviewStatus_NOT_REVIEWED :: ReviewStatus
pattern ReviewStatus_NOT_REVIEWED = ReviewStatus' "NOT_REVIEWED"

pattern ReviewStatus_PENDING :: ReviewStatus
pattern ReviewStatus_PENDING = ReviewStatus' "PENDING"

pattern ReviewStatus_REJECTED :: ReviewStatus
pattern ReviewStatus_REJECTED = ReviewStatus' "REJECTED"

{-# COMPLETE
  ReviewStatus_APPROVED,
  ReviewStatus_NOT_REVIEWED,
  ReviewStatus_PENDING,
  ReviewStatus_REJECTED,
  ReviewStatus'
  #-}
