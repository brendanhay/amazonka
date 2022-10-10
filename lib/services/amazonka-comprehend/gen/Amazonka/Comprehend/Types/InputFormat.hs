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
-- Module      : Amazonka.Comprehend.Types.InputFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.InputFormat
  ( InputFormat
      ( ..,
        InputFormat_ONE_DOC_PER_FILE,
        InputFormat_ONE_DOC_PER_LINE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype InputFormat = InputFormat'
  { fromInputFormat ::
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

pattern InputFormat_ONE_DOC_PER_FILE :: InputFormat
pattern InputFormat_ONE_DOC_PER_FILE = InputFormat' "ONE_DOC_PER_FILE"

pattern InputFormat_ONE_DOC_PER_LINE :: InputFormat
pattern InputFormat_ONE_DOC_PER_LINE = InputFormat' "ONE_DOC_PER_LINE"

{-# COMPLETE
  InputFormat_ONE_DOC_PER_FILE,
  InputFormat_ONE_DOC_PER_LINE,
  InputFormat'
  #-}
