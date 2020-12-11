-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupCertificateMode
  ( SmoothGroupCertificateMode
      ( SmoothGroupCertificateMode',
        SGCMSelfSigned,
        SGCMVerifyAuthenticity
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Smooth Group Certificate Mode
newtype SmoothGroupCertificateMode = SmoothGroupCertificateMode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SGCMSelfSigned :: SmoothGroupCertificateMode
pattern SGCMSelfSigned = SmoothGroupCertificateMode' "SELF_SIGNED"

pattern SGCMVerifyAuthenticity :: SmoothGroupCertificateMode
pattern SGCMVerifyAuthenticity = SmoothGroupCertificateMode' "VERIFY_AUTHENTICITY"

{-# COMPLETE
  SGCMSelfSigned,
  SGCMVerifyAuthenticity,
  SmoothGroupCertificateMode'
  #-}
