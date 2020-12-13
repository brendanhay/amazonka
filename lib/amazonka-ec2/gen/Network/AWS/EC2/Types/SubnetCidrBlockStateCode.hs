{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetCidrBlockStateCode
  ( SubnetCidrBlockStateCode
      ( SubnetCidrBlockStateCode',
        SCBSCAssociating,
        SCBSCAssociated,
        SCBSCDisassociating,
        SCBSCDisassociated,
        SCBSCFailing,
        SCBSCFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SubnetCidrBlockStateCode = SubnetCidrBlockStateCode' Lude.Text
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

pattern SCBSCAssociating :: SubnetCidrBlockStateCode
pattern SCBSCAssociating = SubnetCidrBlockStateCode' "associating"

pattern SCBSCAssociated :: SubnetCidrBlockStateCode
pattern SCBSCAssociated = SubnetCidrBlockStateCode' "associated"

pattern SCBSCDisassociating :: SubnetCidrBlockStateCode
pattern SCBSCDisassociating = SubnetCidrBlockStateCode' "disassociating"

pattern SCBSCDisassociated :: SubnetCidrBlockStateCode
pattern SCBSCDisassociated = SubnetCidrBlockStateCode' "disassociated"

pattern SCBSCFailing :: SubnetCidrBlockStateCode
pattern SCBSCFailing = SubnetCidrBlockStateCode' "failing"

pattern SCBSCFailed :: SubnetCidrBlockStateCode
pattern SCBSCFailed = SubnetCidrBlockStateCode' "failed"

{-# COMPLETE
  SCBSCAssociating,
  SCBSCAssociated,
  SCBSCDisassociating,
  SCBSCDisassociated,
  SCBSCFailing,
  SCBSCFailed,
  SubnetCidrBlockStateCode'
  #-}
