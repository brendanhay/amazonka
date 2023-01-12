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
-- Module      : Amazonka.SageMaker.Types.ModelCardStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardStatus
  ( ModelCardStatus
      ( ..,
        ModelCardStatus_Approved,
        ModelCardStatus_Archived,
        ModelCardStatus_Draft,
        ModelCardStatus_PendingReview
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelCardStatus = ModelCardStatus'
  { fromModelCardStatus ::
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

pattern ModelCardStatus_Approved :: ModelCardStatus
pattern ModelCardStatus_Approved = ModelCardStatus' "Approved"

pattern ModelCardStatus_Archived :: ModelCardStatus
pattern ModelCardStatus_Archived = ModelCardStatus' "Archived"

pattern ModelCardStatus_Draft :: ModelCardStatus
pattern ModelCardStatus_Draft = ModelCardStatus' "Draft"

pattern ModelCardStatus_PendingReview :: ModelCardStatus
pattern ModelCardStatus_PendingReview = ModelCardStatus' "PendingReview"

{-# COMPLETE
  ModelCardStatus_Approved,
  ModelCardStatus_Archived,
  ModelCardStatus_Draft,
  ModelCardStatus_PendingReview,
  ModelCardStatus'
  #-}
