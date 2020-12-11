-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ViewerProtocolPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ViewerProtocolPolicy
  ( ViewerProtocolPolicy
      ( ViewerProtocolPolicy',
        VPPAllowAll,
        VPPHTTPSOnly,
        VPPRedirectToHTTPS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ViewerProtocolPolicy = ViewerProtocolPolicy' Lude.Text
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

pattern VPPAllowAll :: ViewerProtocolPolicy
pattern VPPAllowAll = ViewerProtocolPolicy' "allow-all"

pattern VPPHTTPSOnly :: ViewerProtocolPolicy
pattern VPPHTTPSOnly = ViewerProtocolPolicy' "https-only"

pattern VPPRedirectToHTTPS :: ViewerProtocolPolicy
pattern VPPRedirectToHTTPS = ViewerProtocolPolicy' "redirect-to-https"

{-# COMPLETE
  VPPAllowAll,
  VPPHTTPSOnly,
  VPPRedirectToHTTPS,
  ViewerProtocolPolicy'
  #-}
