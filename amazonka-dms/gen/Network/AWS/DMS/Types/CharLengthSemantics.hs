{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.CharLengthSemantics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.CharLengthSemantics
  ( CharLengthSemantics
      ( ..,
        CharLengthSemantics_Byte,
        CharLengthSemantics_Char,
        CharLengthSemantics_Default
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CharLengthSemantics = CharLengthSemantics'
  { fromCharLengthSemantics ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
