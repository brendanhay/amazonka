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
-- Module      : Amazonka.QuickSight.Types.TextQualifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TextQualifier
  ( TextQualifier
      ( ..,
        TextQualifier_DOUBLE_QUOTE,
        TextQualifier_SINGLE_QUOTE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TextQualifier = TextQualifier'
  { fromTextQualifier ::
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

pattern TextQualifier_DOUBLE_QUOTE :: TextQualifier
pattern TextQualifier_DOUBLE_QUOTE = TextQualifier' "DOUBLE_QUOTE"

pattern TextQualifier_SINGLE_QUOTE :: TextQualifier
pattern TextQualifier_SINGLE_QUOTE = TextQualifier' "SINGLE_QUOTE"

{-# COMPLETE
  TextQualifier_DOUBLE_QUOTE,
  TextQualifier_SINGLE_QUOTE,
  TextQualifier'
  #-}
