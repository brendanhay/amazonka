{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.LocaleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.LocaleType
  ( LocaleType
    ( LocaleType'
    , LocaleTypeEN
    , LocaleTypeFR
    , LocaleTypeKO
    , LocaleTypeDE
    , LocaleTypeES
    , LocaleTypeJA
    , LocaleTypeRU
    , LocaleTypeZhCn
    , LocaleTypeZhTw
    , LocaleTypePtBr
    , LocaleTypeDefault
    , fromLocaleType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LocaleType = LocaleType'{fromLocaleType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern LocaleTypeEN :: LocaleType
pattern LocaleTypeEN = LocaleType' "en"

pattern LocaleTypeFR :: LocaleType
pattern LocaleTypeFR = LocaleType' "fr"

pattern LocaleTypeKO :: LocaleType
pattern LocaleTypeKO = LocaleType' "ko"

pattern LocaleTypeDE :: LocaleType
pattern LocaleTypeDE = LocaleType' "de"

pattern LocaleTypeES :: LocaleType
pattern LocaleTypeES = LocaleType' "es"

pattern LocaleTypeJA :: LocaleType
pattern LocaleTypeJA = LocaleType' "ja"

pattern LocaleTypeRU :: LocaleType
pattern LocaleTypeRU = LocaleType' "ru"

pattern LocaleTypeZhCn :: LocaleType
pattern LocaleTypeZhCn = LocaleType' "zh_CN"

pattern LocaleTypeZhTw :: LocaleType
pattern LocaleTypeZhTw = LocaleType' "zh_TW"

pattern LocaleTypePtBr :: LocaleType
pattern LocaleTypePtBr = LocaleType' "pt_BR"

pattern LocaleTypeDefault :: LocaleType
pattern LocaleTypeDefault = LocaleType' "default"

{-# COMPLETE 
  LocaleTypeEN,

  LocaleTypeFR,

  LocaleTypeKO,

  LocaleTypeDE,

  LocaleTypeES,

  LocaleTypeJA,

  LocaleTypeRU,

  LocaleTypeZhCn,

  LocaleTypeZhTw,

  LocaleTypePtBr,

  LocaleTypeDefault,
  LocaleType'
  #-}
