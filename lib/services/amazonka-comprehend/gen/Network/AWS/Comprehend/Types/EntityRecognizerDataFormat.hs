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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerDataFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerDataFormat
  ( EntityRecognizerDataFormat
      ( ..,
        EntityRecognizerDataFormat_AUGMENTED_MANIFEST,
        EntityRecognizerDataFormat_COMPREHEND_CSV
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EntityRecognizerDataFormat = EntityRecognizerDataFormat'
  { fromEntityRecognizerDataFormat ::
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

pattern EntityRecognizerDataFormat_AUGMENTED_MANIFEST :: EntityRecognizerDataFormat
pattern EntityRecognizerDataFormat_AUGMENTED_MANIFEST = EntityRecognizerDataFormat' "AUGMENTED_MANIFEST"

pattern EntityRecognizerDataFormat_COMPREHEND_CSV :: EntityRecognizerDataFormat
pattern EntityRecognizerDataFormat_COMPREHEND_CSV = EntityRecognizerDataFormat' "COMPREHEND_CSV"

{-# COMPLETE
  EntityRecognizerDataFormat_AUGMENTED_MANIFEST,
  EntityRecognizerDataFormat_COMPREHEND_CSV,
  EntityRecognizerDataFormat'
  #-}
