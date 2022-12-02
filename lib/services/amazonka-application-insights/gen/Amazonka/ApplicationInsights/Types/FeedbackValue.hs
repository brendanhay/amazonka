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
-- Module      : Amazonka.ApplicationInsights.Types.FeedbackValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.FeedbackValue
  ( FeedbackValue
      ( ..,
        FeedbackValue_NOT_SPECIFIED,
        FeedbackValue_NOT_USEFUL,
        FeedbackValue_USEFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FeedbackValue = FeedbackValue'
  { fromFeedbackValue ::
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

pattern FeedbackValue_NOT_SPECIFIED :: FeedbackValue
pattern FeedbackValue_NOT_SPECIFIED = FeedbackValue' "NOT_SPECIFIED"

pattern FeedbackValue_NOT_USEFUL :: FeedbackValue
pattern FeedbackValue_NOT_USEFUL = FeedbackValue' "NOT_USEFUL"

pattern FeedbackValue_USEFUL :: FeedbackValue
pattern FeedbackValue_USEFUL = FeedbackValue' "USEFUL"

{-# COMPLETE
  FeedbackValue_NOT_SPECIFIED,
  FeedbackValue_NOT_USEFUL,
  FeedbackValue_USEFUL,
  FeedbackValue'
  #-}
