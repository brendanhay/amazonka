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
-- Module      : Amazonka.DMS.Types.CharLengthSemantics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.CharLengthSemantics
  ( CharLengthSemantics
      ( ..,
        CharLengthSemantics_Byte,
        CharLengthSemantics_Char,
        CharLengthSemantics_Default
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CharLengthSemantics = CharLengthSemantics'
  { fromCharLengthSemantics ::
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

pattern CharLengthSemantics_Byte :: CharLengthSemantics
pattern CharLengthSemantics_Byte = CharLengthSemantics' "byte"

pattern CharLengthSemantics_Char :: CharLengthSemantics
pattern CharLengthSemantics_Char = CharLengthSemantics' "char"

pattern CharLengthSemantics_Default :: CharLengthSemantics
pattern CharLengthSemantics_Default = CharLengthSemantics' "default"

{-# COMPLETE
  CharLengthSemantics_Byte,
  CharLengthSemantics_Char,
  CharLengthSemantics_Default,
  CharLengthSemantics'
  #-}
