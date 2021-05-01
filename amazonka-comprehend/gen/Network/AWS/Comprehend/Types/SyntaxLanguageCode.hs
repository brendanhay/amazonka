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
-- Module      : Network.AWS.Comprehend.Types.SyntaxLanguageCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SyntaxLanguageCode
  ( SyntaxLanguageCode
      ( ..,
        SyntaxLanguageCode_De,
        SyntaxLanguageCode_En,
        SyntaxLanguageCode_Es,
        SyntaxLanguageCode_Fr,
        SyntaxLanguageCode_It,
        SyntaxLanguageCode_Pt
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SyntaxLanguageCode = SyntaxLanguageCode'
  { fromSyntaxLanguageCode ::
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

pattern SyntaxLanguageCode_De :: SyntaxLanguageCode
pattern SyntaxLanguageCode_De = SyntaxLanguageCode' "de"

pattern SyntaxLanguageCode_En :: SyntaxLanguageCode
pattern SyntaxLanguageCode_En = SyntaxLanguageCode' "en"

pattern SyntaxLanguageCode_Es :: SyntaxLanguageCode
pattern SyntaxLanguageCode_Es = SyntaxLanguageCode' "es"

pattern SyntaxLanguageCode_Fr :: SyntaxLanguageCode
pattern SyntaxLanguageCode_Fr = SyntaxLanguageCode' "fr"

pattern SyntaxLanguageCode_It :: SyntaxLanguageCode
pattern SyntaxLanguageCode_It = SyntaxLanguageCode' "it"

pattern SyntaxLanguageCode_Pt :: SyntaxLanguageCode
pattern SyntaxLanguageCode_Pt = SyntaxLanguageCode' "pt"

{-# COMPLETE
  SyntaxLanguageCode_De,
  SyntaxLanguageCode_En,
  SyntaxLanguageCode_Es,
  SyntaxLanguageCode_Fr,
  SyntaxLanguageCode_It,
  SyntaxLanguageCode_Pt,
  SyntaxLanguageCode'
  #-}
