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
-- Module      : Network.AWS.FraudDetector.Types.ModelOutputDataFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.ModelOutputDataFormat
  ( ModelOutputDataFormat
      ( ..,
        ModelOutputDataFormat_APPLICATION_JSONLINES,
        ModelOutputDataFormat_TEXT_CSV
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ModelOutputDataFormat = ModelOutputDataFormat'
  { fromModelOutputDataFormat ::
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

pattern ModelOutputDataFormat_APPLICATION_JSONLINES :: ModelOutputDataFormat
pattern ModelOutputDataFormat_APPLICATION_JSONLINES = ModelOutputDataFormat' "APPLICATION_JSONLINES"

pattern ModelOutputDataFormat_TEXT_CSV :: ModelOutputDataFormat
pattern ModelOutputDataFormat_TEXT_CSV = ModelOutputDataFormat' "TEXT_CSV"

{-# COMPLETE
  ModelOutputDataFormat_APPLICATION_JSONLINES,
  ModelOutputDataFormat_TEXT_CSV,
  ModelOutputDataFormat'
  #-}
