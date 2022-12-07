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
-- Module      : Amazonka.Kendra.Types.ScoreConfidence
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ScoreConfidence
  ( ScoreConfidence
      ( ..,
        ScoreConfidence_HIGH,
        ScoreConfidence_LOW,
        ScoreConfidence_MEDIUM,
        ScoreConfidence_NOT_AVAILABLE,
        ScoreConfidence_VERY_HIGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enumeration for query score confidence.
newtype ScoreConfidence = ScoreConfidence'
  { fromScoreConfidence ::
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

pattern ScoreConfidence_HIGH :: ScoreConfidence
pattern ScoreConfidence_HIGH = ScoreConfidence' "HIGH"

pattern ScoreConfidence_LOW :: ScoreConfidence
pattern ScoreConfidence_LOW = ScoreConfidence' "LOW"

pattern ScoreConfidence_MEDIUM :: ScoreConfidence
pattern ScoreConfidence_MEDIUM = ScoreConfidence' "MEDIUM"

pattern ScoreConfidence_NOT_AVAILABLE :: ScoreConfidence
pattern ScoreConfidence_NOT_AVAILABLE = ScoreConfidence' "NOT_AVAILABLE"

pattern ScoreConfidence_VERY_HIGH :: ScoreConfidence
pattern ScoreConfidence_VERY_HIGH = ScoreConfidence' "VERY_HIGH"

{-# COMPLETE
  ScoreConfidence_HIGH,
  ScoreConfidence_LOW,
  ScoreConfidence_MEDIUM,
  ScoreConfidence_NOT_AVAILABLE,
  ScoreConfidence_VERY_HIGH,
  ScoreConfidence'
  #-}
