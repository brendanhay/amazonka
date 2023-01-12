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
-- Module      : Amazonka.MechanicalTurk.Types.ReviewActionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.ReviewActionStatus
  ( ReviewActionStatus
      ( ..,
        ReviewActionStatus_Cancelled,
        ReviewActionStatus_Failed,
        ReviewActionStatus_Intended,
        ReviewActionStatus_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReviewActionStatus = ReviewActionStatus'
  { fromReviewActionStatus ::
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

pattern ReviewActionStatus_Cancelled :: ReviewActionStatus
pattern ReviewActionStatus_Cancelled = ReviewActionStatus' "Cancelled"

pattern ReviewActionStatus_Failed :: ReviewActionStatus
pattern ReviewActionStatus_Failed = ReviewActionStatus' "Failed"

pattern ReviewActionStatus_Intended :: ReviewActionStatus
pattern ReviewActionStatus_Intended = ReviewActionStatus' "Intended"

pattern ReviewActionStatus_Succeeded :: ReviewActionStatus
pattern ReviewActionStatus_Succeeded = ReviewActionStatus' "Succeeded"

{-# COMPLETE
  ReviewActionStatus_Cancelled,
  ReviewActionStatus_Failed,
  ReviewActionStatus_Intended,
  ReviewActionStatus_Succeeded,
  ReviewActionStatus'
  #-}
