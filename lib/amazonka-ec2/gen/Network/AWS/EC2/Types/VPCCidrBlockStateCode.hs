-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCCidrBlockStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCCidrBlockStateCode
  ( VPCCidrBlockStateCode
      ( VPCCidrBlockStateCode',
        VCBSCAssociated,
        VCBSCAssociating,
        VCBSCDisassociated,
        VCBSCDisassociating,
        VCBSCFailed,
        VCBSCFailing
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VPCCidrBlockStateCode = VPCCidrBlockStateCode' Lude.Text
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

pattern VCBSCAssociated :: VPCCidrBlockStateCode
pattern VCBSCAssociated = VPCCidrBlockStateCode' "associated"

pattern VCBSCAssociating :: VPCCidrBlockStateCode
pattern VCBSCAssociating = VPCCidrBlockStateCode' "associating"

pattern VCBSCDisassociated :: VPCCidrBlockStateCode
pattern VCBSCDisassociated = VPCCidrBlockStateCode' "disassociated"

pattern VCBSCDisassociating :: VPCCidrBlockStateCode
pattern VCBSCDisassociating = VPCCidrBlockStateCode' "disassociating"

pattern VCBSCFailed :: VPCCidrBlockStateCode
pattern VCBSCFailed = VPCCidrBlockStateCode' "failed"

pattern VCBSCFailing :: VPCCidrBlockStateCode
pattern VCBSCFailing = VPCCidrBlockStateCode' "failing"

{-# COMPLETE
  VCBSCAssociated,
  VCBSCAssociating,
  VCBSCDisassociated,
  VCBSCDisassociating,
  VCBSCFailed,
  VCBSCFailing,
  VPCCidrBlockStateCode'
  #-}
