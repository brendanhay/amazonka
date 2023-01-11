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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerDataFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerDataFormat
  ( EntityRecognizerDataFormat
      ( ..,
        EntityRecognizerDataFormat_AUGMENTED_MANIFEST,
        EntityRecognizerDataFormat_COMPREHEND_CSV
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EntityRecognizerDataFormat = EntityRecognizerDataFormat'
  { fromEntityRecognizerDataFormat ::
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

pattern EntityRecognizerDataFormat_AUGMENTED_MANIFEST :: EntityRecognizerDataFormat
pattern EntityRecognizerDataFormat_AUGMENTED_MANIFEST = EntityRecognizerDataFormat' "AUGMENTED_MANIFEST"

pattern EntityRecognizerDataFormat_COMPREHEND_CSV :: EntityRecognizerDataFormat
pattern EntityRecognizerDataFormat_COMPREHEND_CSV = EntityRecognizerDataFormat' "COMPREHEND_CSV"

{-# COMPLETE
  EntityRecognizerDataFormat_AUGMENTED_MANIFEST,
  EntityRecognizerDataFormat_COMPREHEND_CSV,
  EntityRecognizerDataFormat'
  #-}
