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
-- Module      : Amazonka.MediaConvert.Types.XavcProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcProfile
  ( XavcProfile
      ( ..,
        XavcProfile_XAVC_4K,
        XavcProfile_XAVC_4K_INTRA_CBG,
        XavcProfile_XAVC_4K_INTRA_VBR,
        XavcProfile_XAVC_HD,
        XavcProfile_XAVC_HD_INTRA_CBG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the XAVC profile for this output. For more information, see the
-- Sony documentation at https:\/\/www.xavc-info.org\/. Note that
-- MediaConvert doesn\'t support the interlaced video XAVC operating points
-- for XAVC_HD_INTRA_CBG. To create an interlaced XAVC output, choose the
-- profile XAVC_HD.
newtype XavcProfile = XavcProfile'
  { fromXavcProfile ::
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

pattern XavcProfile_XAVC_4K :: XavcProfile
pattern XavcProfile_XAVC_4K = XavcProfile' "XAVC_4K"

pattern XavcProfile_XAVC_4K_INTRA_CBG :: XavcProfile
pattern XavcProfile_XAVC_4K_INTRA_CBG = XavcProfile' "XAVC_4K_INTRA_CBG"

pattern XavcProfile_XAVC_4K_INTRA_VBR :: XavcProfile
pattern XavcProfile_XAVC_4K_INTRA_VBR = XavcProfile' "XAVC_4K_INTRA_VBR"

pattern XavcProfile_XAVC_HD :: XavcProfile
pattern XavcProfile_XAVC_HD = XavcProfile' "XAVC_HD"

pattern XavcProfile_XAVC_HD_INTRA_CBG :: XavcProfile
pattern XavcProfile_XAVC_HD_INTRA_CBG = XavcProfile' "XAVC_HD_INTRA_CBG"

{-# COMPLETE
  XavcProfile_XAVC_4K,
  XavcProfile_XAVC_4K_INTRA_CBG,
  XavcProfile_XAVC_4K_INTRA_VBR,
  XavcProfile_XAVC_HD,
  XavcProfile_XAVC_HD_INTRA_CBG,
  XavcProfile'
  #-}
