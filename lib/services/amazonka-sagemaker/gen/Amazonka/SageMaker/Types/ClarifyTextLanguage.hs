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
-- Module      : Amazonka.SageMaker.Types.ClarifyTextLanguage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ClarifyTextLanguage
  ( ClarifyTextLanguage
      ( ..,
        ClarifyTextLanguage_Af,
        ClarifyTextLanguage_Ar,
        ClarifyTextLanguage_Bg,
        ClarifyTextLanguage_Bn,
        ClarifyTextLanguage_Ca,
        ClarifyTextLanguage_Cs,
        ClarifyTextLanguage_Da,
        ClarifyTextLanguage_De,
        ClarifyTextLanguage_El,
        ClarifyTextLanguage_En,
        ClarifyTextLanguage_Es,
        ClarifyTextLanguage_Et,
        ClarifyTextLanguage_Eu,
        ClarifyTextLanguage_Fa,
        ClarifyTextLanguage_Fi,
        ClarifyTextLanguage_Fr,
        ClarifyTextLanguage_Ga,
        ClarifyTextLanguage_Gu,
        ClarifyTextLanguage_He,
        ClarifyTextLanguage_Hi,
        ClarifyTextLanguage_Hr,
        ClarifyTextLanguage_Hu,
        ClarifyTextLanguage_Hy,
        ClarifyTextLanguage_Id,
        ClarifyTextLanguage_Is,
        ClarifyTextLanguage_It,
        ClarifyTextLanguage_Kn,
        ClarifyTextLanguage_Ky,
        ClarifyTextLanguage_Lb,
        ClarifyTextLanguage_Lij,
        ClarifyTextLanguage_Lt,
        ClarifyTextLanguage_Lv,
        ClarifyTextLanguage_Mk,
        ClarifyTextLanguage_Ml,
        ClarifyTextLanguage_Mr,
        ClarifyTextLanguage_Nb,
        ClarifyTextLanguage_Ne,
        ClarifyTextLanguage_Nl,
        ClarifyTextLanguage_Pl,
        ClarifyTextLanguage_Pt,
        ClarifyTextLanguage_Ro,
        ClarifyTextLanguage_Ru,
        ClarifyTextLanguage_Sa,
        ClarifyTextLanguage_Si,
        ClarifyTextLanguage_Sk,
        ClarifyTextLanguage_Sl,
        ClarifyTextLanguage_Sq,
        ClarifyTextLanguage_Sr,
        ClarifyTextLanguage_Sv,
        ClarifyTextLanguage_Ta,
        ClarifyTextLanguage_Te,
        ClarifyTextLanguage_Tl,
        ClarifyTextLanguage_Tn,
        ClarifyTextLanguage_Tr,
        ClarifyTextLanguage_Tt,
        ClarifyTextLanguage_Uk,
        ClarifyTextLanguage_Ur,
        ClarifyTextLanguage_Xx,
        ClarifyTextLanguage_Yo,
        ClarifyTextLanguage_Zh
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClarifyTextLanguage = ClarifyTextLanguage'
  { fromClarifyTextLanguage ::
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

pattern ClarifyTextLanguage_Af :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Af = ClarifyTextLanguage' "af"

pattern ClarifyTextLanguage_Ar :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ar = ClarifyTextLanguage' "ar"

pattern ClarifyTextLanguage_Bg :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Bg = ClarifyTextLanguage' "bg"

pattern ClarifyTextLanguage_Bn :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Bn = ClarifyTextLanguage' "bn"

pattern ClarifyTextLanguage_Ca :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ca = ClarifyTextLanguage' "ca"

pattern ClarifyTextLanguage_Cs :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Cs = ClarifyTextLanguage' "cs"

pattern ClarifyTextLanguage_Da :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Da = ClarifyTextLanguage' "da"

pattern ClarifyTextLanguage_De :: ClarifyTextLanguage
pattern ClarifyTextLanguage_De = ClarifyTextLanguage' "de"

pattern ClarifyTextLanguage_El :: ClarifyTextLanguage
pattern ClarifyTextLanguage_El = ClarifyTextLanguage' "el"

pattern ClarifyTextLanguage_En :: ClarifyTextLanguage
pattern ClarifyTextLanguage_En = ClarifyTextLanguage' "en"

pattern ClarifyTextLanguage_Es :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Es = ClarifyTextLanguage' "es"

pattern ClarifyTextLanguage_Et :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Et = ClarifyTextLanguage' "et"

pattern ClarifyTextLanguage_Eu :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Eu = ClarifyTextLanguage' "eu"

pattern ClarifyTextLanguage_Fa :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Fa = ClarifyTextLanguage' "fa"

pattern ClarifyTextLanguage_Fi :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Fi = ClarifyTextLanguage' "fi"

pattern ClarifyTextLanguage_Fr :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Fr = ClarifyTextLanguage' "fr"

pattern ClarifyTextLanguage_Ga :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ga = ClarifyTextLanguage' "ga"

pattern ClarifyTextLanguage_Gu :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Gu = ClarifyTextLanguage' "gu"

pattern ClarifyTextLanguage_He :: ClarifyTextLanguage
pattern ClarifyTextLanguage_He = ClarifyTextLanguage' "he"

pattern ClarifyTextLanguage_Hi :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Hi = ClarifyTextLanguage' "hi"

pattern ClarifyTextLanguage_Hr :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Hr = ClarifyTextLanguage' "hr"

pattern ClarifyTextLanguage_Hu :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Hu = ClarifyTextLanguage' "hu"

pattern ClarifyTextLanguage_Hy :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Hy = ClarifyTextLanguage' "hy"

pattern ClarifyTextLanguage_Id :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Id = ClarifyTextLanguage' "id"

pattern ClarifyTextLanguage_Is :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Is = ClarifyTextLanguage' "is"

pattern ClarifyTextLanguage_It :: ClarifyTextLanguage
pattern ClarifyTextLanguage_It = ClarifyTextLanguage' "it"

pattern ClarifyTextLanguage_Kn :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Kn = ClarifyTextLanguage' "kn"

pattern ClarifyTextLanguage_Ky :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ky = ClarifyTextLanguage' "ky"

pattern ClarifyTextLanguage_Lb :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Lb = ClarifyTextLanguage' "lb"

pattern ClarifyTextLanguage_Lij :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Lij = ClarifyTextLanguage' "lij"

pattern ClarifyTextLanguage_Lt :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Lt = ClarifyTextLanguage' "lt"

pattern ClarifyTextLanguage_Lv :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Lv = ClarifyTextLanguage' "lv"

pattern ClarifyTextLanguage_Mk :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Mk = ClarifyTextLanguage' "mk"

pattern ClarifyTextLanguage_Ml :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ml = ClarifyTextLanguage' "ml"

pattern ClarifyTextLanguage_Mr :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Mr = ClarifyTextLanguage' "mr"

pattern ClarifyTextLanguage_Nb :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Nb = ClarifyTextLanguage' "nb"

pattern ClarifyTextLanguage_Ne :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ne = ClarifyTextLanguage' "ne"

pattern ClarifyTextLanguage_Nl :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Nl = ClarifyTextLanguage' "nl"

pattern ClarifyTextLanguage_Pl :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Pl = ClarifyTextLanguage' "pl"

pattern ClarifyTextLanguage_Pt :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Pt = ClarifyTextLanguage' "pt"

pattern ClarifyTextLanguage_Ro :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ro = ClarifyTextLanguage' "ro"

pattern ClarifyTextLanguage_Ru :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ru = ClarifyTextLanguage' "ru"

pattern ClarifyTextLanguage_Sa :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Sa = ClarifyTextLanguage' "sa"

pattern ClarifyTextLanguage_Si :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Si = ClarifyTextLanguage' "si"

pattern ClarifyTextLanguage_Sk :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Sk = ClarifyTextLanguage' "sk"

pattern ClarifyTextLanguage_Sl :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Sl = ClarifyTextLanguage' "sl"

pattern ClarifyTextLanguage_Sq :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Sq = ClarifyTextLanguage' "sq"

pattern ClarifyTextLanguage_Sr :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Sr = ClarifyTextLanguage' "sr"

pattern ClarifyTextLanguage_Sv :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Sv = ClarifyTextLanguage' "sv"

pattern ClarifyTextLanguage_Ta :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ta = ClarifyTextLanguage' "ta"

pattern ClarifyTextLanguage_Te :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Te = ClarifyTextLanguage' "te"

pattern ClarifyTextLanguage_Tl :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Tl = ClarifyTextLanguage' "tl"

pattern ClarifyTextLanguage_Tn :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Tn = ClarifyTextLanguage' "tn"

pattern ClarifyTextLanguage_Tr :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Tr = ClarifyTextLanguage' "tr"

pattern ClarifyTextLanguage_Tt :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Tt = ClarifyTextLanguage' "tt"

pattern ClarifyTextLanguage_Uk :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Uk = ClarifyTextLanguage' "uk"

pattern ClarifyTextLanguage_Ur :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Ur = ClarifyTextLanguage' "ur"

pattern ClarifyTextLanguage_Xx :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Xx = ClarifyTextLanguage' "xx"

pattern ClarifyTextLanguage_Yo :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Yo = ClarifyTextLanguage' "yo"

pattern ClarifyTextLanguage_Zh :: ClarifyTextLanguage
pattern ClarifyTextLanguage_Zh = ClarifyTextLanguage' "zh"

{-# COMPLETE
  ClarifyTextLanguage_Af,
  ClarifyTextLanguage_Ar,
  ClarifyTextLanguage_Bg,
  ClarifyTextLanguage_Bn,
  ClarifyTextLanguage_Ca,
  ClarifyTextLanguage_Cs,
  ClarifyTextLanguage_Da,
  ClarifyTextLanguage_De,
  ClarifyTextLanguage_El,
  ClarifyTextLanguage_En,
  ClarifyTextLanguage_Es,
  ClarifyTextLanguage_Et,
  ClarifyTextLanguage_Eu,
  ClarifyTextLanguage_Fa,
  ClarifyTextLanguage_Fi,
  ClarifyTextLanguage_Fr,
  ClarifyTextLanguage_Ga,
  ClarifyTextLanguage_Gu,
  ClarifyTextLanguage_He,
  ClarifyTextLanguage_Hi,
  ClarifyTextLanguage_Hr,
  ClarifyTextLanguage_Hu,
  ClarifyTextLanguage_Hy,
  ClarifyTextLanguage_Id,
  ClarifyTextLanguage_Is,
  ClarifyTextLanguage_It,
  ClarifyTextLanguage_Kn,
  ClarifyTextLanguage_Ky,
  ClarifyTextLanguage_Lb,
  ClarifyTextLanguage_Lij,
  ClarifyTextLanguage_Lt,
  ClarifyTextLanguage_Lv,
  ClarifyTextLanguage_Mk,
  ClarifyTextLanguage_Ml,
  ClarifyTextLanguage_Mr,
  ClarifyTextLanguage_Nb,
  ClarifyTextLanguage_Ne,
  ClarifyTextLanguage_Nl,
  ClarifyTextLanguage_Pl,
  ClarifyTextLanguage_Pt,
  ClarifyTextLanguage_Ro,
  ClarifyTextLanguage_Ru,
  ClarifyTextLanguage_Sa,
  ClarifyTextLanguage_Si,
  ClarifyTextLanguage_Sk,
  ClarifyTextLanguage_Sl,
  ClarifyTextLanguage_Sq,
  ClarifyTextLanguage_Sr,
  ClarifyTextLanguage_Sv,
  ClarifyTextLanguage_Ta,
  ClarifyTextLanguage_Te,
  ClarifyTextLanguage_Tl,
  ClarifyTextLanguage_Tn,
  ClarifyTextLanguage_Tr,
  ClarifyTextLanguage_Tt,
  ClarifyTextLanguage_Uk,
  ClarifyTextLanguage_Ur,
  ClarifyTextLanguage_Xx,
  ClarifyTextLanguage_Yo,
  ClarifyTextLanguage_Zh,
  ClarifyTextLanguage'
  #-}
