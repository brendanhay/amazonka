{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        VCBSCAssociating,
        VCBSCAssociated,
        VCBSCDisassociating,
        VCBSCDisassociated,
        VCBSCFailing,
        VCBSCFailed
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

pattern VCBSCAssociating :: VPCCidrBlockStateCode
pattern VCBSCAssociating = VPCCidrBlockStateCode' "associating"

pattern VCBSCAssociated :: VPCCidrBlockStateCode
pattern VCBSCAssociated = VPCCidrBlockStateCode' "associated"

pattern VCBSCDisassociating :: VPCCidrBlockStateCode
pattern VCBSCDisassociating = VPCCidrBlockStateCode' "disassociating"

pattern VCBSCDisassociated :: VPCCidrBlockStateCode
pattern VCBSCDisassociated = VPCCidrBlockStateCode' "disassociated"

pattern VCBSCFailing :: VPCCidrBlockStateCode
pattern VCBSCFailing = VPCCidrBlockStateCode' "failing"

pattern VCBSCFailed :: VPCCidrBlockStateCode
pattern VCBSCFailed = VPCCidrBlockStateCode' "failed"

{-# COMPLETE
  VCBSCAssociating,
  VCBSCAssociated,
  VCBSCDisassociating,
  VCBSCDisassociated,
  VCBSCFailing,
  VCBSCFailed,
  VPCCidrBlockStateCode'
  #-}
