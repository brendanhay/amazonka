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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CharLengthSemantics = CharLengthSemantics'
  { fromCharLengthSemantics ::
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
