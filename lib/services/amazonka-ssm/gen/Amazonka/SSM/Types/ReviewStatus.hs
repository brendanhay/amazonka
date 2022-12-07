{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Types.ReviewStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ReviewStatus
  ( ReviewStatus
      ( ..,
        ReviewStatus_APPROVED,
        ReviewStatus_NOT_REVIEWED,
        ReviewStatus_PENDING,
        ReviewStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReviewStatus = ReviewStatus'
  { fromReviewStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
