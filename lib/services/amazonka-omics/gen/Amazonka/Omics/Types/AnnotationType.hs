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
-- Module      : Amazonka.Omics.Types.AnnotationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.AnnotationType
  ( AnnotationType
      ( ..,
        AnnotationType_CHR_POS,
        AnnotationType_CHR_POS_REF_ALT,
        AnnotationType_CHR_START_END_ONE_BASE,
        AnnotationType_CHR_START_END_REF_ALT_ONE_BASE,
        AnnotationType_CHR_START_END_REF_ALT_ZERO_BASE,
        AnnotationType_CHR_START_END_ZERO_BASE,
        AnnotationType_GENERIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnnotationType = AnnotationType'
  { fromAnnotationType ::
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

pattern AnnotationType_CHR_POS :: AnnotationType
pattern AnnotationType_CHR_POS = AnnotationType' "CHR_POS"

pattern AnnotationType_CHR_POS_REF_ALT :: AnnotationType
pattern AnnotationType_CHR_POS_REF_ALT = AnnotationType' "CHR_POS_REF_ALT"

pattern AnnotationType_CHR_START_END_ONE_BASE :: AnnotationType
pattern AnnotationType_CHR_START_END_ONE_BASE = AnnotationType' "CHR_START_END_ONE_BASE"

pattern AnnotationType_CHR_START_END_REF_ALT_ONE_BASE :: AnnotationType
pattern AnnotationType_CHR_START_END_REF_ALT_ONE_BASE = AnnotationType' "CHR_START_END_REF_ALT_ONE_BASE"

pattern AnnotationType_CHR_START_END_REF_ALT_ZERO_BASE :: AnnotationType
pattern AnnotationType_CHR_START_END_REF_ALT_ZERO_BASE = AnnotationType' "CHR_START_END_REF_ALT_ZERO_BASE"

pattern AnnotationType_CHR_START_END_ZERO_BASE :: AnnotationType
pattern AnnotationType_CHR_START_END_ZERO_BASE = AnnotationType' "CHR_START_END_ZERO_BASE"

pattern AnnotationType_GENERIC :: AnnotationType
pattern AnnotationType_GENERIC = AnnotationType' "GENERIC"

{-# COMPLETE
  AnnotationType_CHR_POS,
  AnnotationType_CHR_POS_REF_ALT,
  AnnotationType_CHR_START_END_ONE_BASE,
  AnnotationType_CHR_START_END_REF_ALT_ONE_BASE,
  AnnotationType_CHR_START_END_REF_ALT_ZERO_BASE,
  AnnotationType_CHR_START_END_ZERO_BASE,
  AnnotationType_GENERIC,
  AnnotationType'
  #-}
