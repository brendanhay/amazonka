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
-- Module      : Amazonka.MediaConvert.Types.MxfProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MxfProfile
  ( MxfProfile
      ( ..,
        MxfProfile_D_10,
        MxfProfile_OP1A,
        MxfProfile_XAVC,
        MxfProfile_XDCAM,
        MxfProfile_XDCAM_RDD9
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
newtype MxfProfile = MxfProfile'
  { fromMxfProfile ::
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

pattern MxfProfile_D_10 :: MxfProfile
pattern MxfProfile_D_10 = MxfProfile' "D_10"

pattern MxfProfile_OP1A :: MxfProfile
pattern MxfProfile_OP1A = MxfProfile' "OP1A"

pattern MxfProfile_XAVC :: MxfProfile
pattern MxfProfile_XAVC = MxfProfile' "XAVC"

pattern MxfProfile_XDCAM :: MxfProfile
pattern MxfProfile_XDCAM = MxfProfile' "XDCAM"

pattern MxfProfile_XDCAM_RDD9 :: MxfProfile
pattern MxfProfile_XDCAM_RDD9 = MxfProfile' "XDCAM_RDD9"

{-# COMPLETE
  MxfProfile_D_10,
  MxfProfile_OP1A,
  MxfProfile_XAVC,
  MxfProfile_XDCAM,
  MxfProfile_XDCAM_RDD9,
  MxfProfile'
  #-}
