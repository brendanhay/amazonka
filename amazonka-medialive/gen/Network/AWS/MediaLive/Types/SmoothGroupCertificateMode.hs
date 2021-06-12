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
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
  ( SmoothGroupCertificateMode
      ( ..,
        SmoothGroupCertificateMode_SELF_SIGNED,
        SmoothGroupCertificateMode_VERIFY_AUTHENTICITY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Smooth Group Certificate Mode
newtype SmoothGroupCertificateMode = SmoothGroupCertificateMode'
  { fromSmoothGroupCertificateMode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern SmoothGroupCertificateMode_SELF_SIGNED :: SmoothGroupCertificateMode
pattern SmoothGroupCertificateMode_SELF_SIGNED = SmoothGroupCertificateMode' "SELF_SIGNED"

pattern SmoothGroupCertificateMode_VERIFY_AUTHENTICITY :: SmoothGroupCertificateMode
pattern SmoothGroupCertificateMode_VERIFY_AUTHENTICITY = SmoothGroupCertificateMode' "VERIFY_AUTHENTICITY"

{-# COMPLETE
  SmoothGroupCertificateMode_SELF_SIGNED,
  SmoothGroupCertificateMode_VERIFY_AUTHENTICITY,
  SmoothGroupCertificateMode'
  #-}
