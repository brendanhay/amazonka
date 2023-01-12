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
-- Module      : Amazonka.Wisdom.Types.RelevanceLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.RelevanceLevel
  ( RelevanceLevel
      ( ..,
        RelevanceLevel_HIGH,
        RelevanceLevel_LOW,
        RelevanceLevel_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RelevanceLevel = RelevanceLevel'
  { fromRelevanceLevel ::
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

pattern RelevanceLevel_HIGH :: RelevanceLevel
pattern RelevanceLevel_HIGH = RelevanceLevel' "HIGH"

pattern RelevanceLevel_LOW :: RelevanceLevel
pattern RelevanceLevel_LOW = RelevanceLevel' "LOW"

pattern RelevanceLevel_MEDIUM :: RelevanceLevel
pattern RelevanceLevel_MEDIUM = RelevanceLevel' "MEDIUM"

{-# COMPLETE
  RelevanceLevel_HIGH,
  RelevanceLevel_LOW,
  RelevanceLevel_MEDIUM,
  RelevanceLevel'
  #-}
