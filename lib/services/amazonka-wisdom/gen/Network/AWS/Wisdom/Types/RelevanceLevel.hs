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
-- Module      : Network.AWS.Wisdom.Types.RelevanceLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Wisdom.Types.RelevanceLevel
  ( RelevanceLevel
      ( ..,
        RelevanceLevel_HIGH,
        RelevanceLevel_LOW,
        RelevanceLevel_MEDIUM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RelevanceLevel = RelevanceLevel'
  { fromRelevanceLevel ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
