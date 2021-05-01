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
-- Module      : Network.AWS.MediaConvert.Types.FontScript
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FontScript
  ( FontScript
      ( ..,
        FontScript_AUTOMATIC,
        FontScript_HANS,
        FontScript_HANT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Provide the font script, using an ISO 15924 script code, if the
-- LanguageCode is not sufficient for determining the script type. Where
-- LanguageCode or CustomLanguageCode is sufficient, use \"AUTOMATIC\" or
-- leave unset.
newtype FontScript = FontScript'
  { fromFontScript ::
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

pattern FontScript_AUTOMATIC :: FontScript
pattern FontScript_AUTOMATIC = FontScript' "AUTOMATIC"

pattern FontScript_HANS :: FontScript
pattern FontScript_HANS = FontScript' "HANS"

pattern FontScript_HANT :: FontScript
pattern FontScript_HANT = FontScript' "HANT"

{-# COMPLETE
  FontScript_AUTOMATIC,
  FontScript_HANS,
  FontScript_HANT,
  FontScript'
  #-}
