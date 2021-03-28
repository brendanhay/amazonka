{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SubnetCidrBlockStateCode
  ( SubnetCidrBlockStateCode
    ( SubnetCidrBlockStateCode'
    , SubnetCidrBlockStateCodeAssociating
    , SubnetCidrBlockStateCodeAssociated
    , SubnetCidrBlockStateCodeDisassociating
    , SubnetCidrBlockStateCodeDisassociated
    , SubnetCidrBlockStateCodeFailing
    , SubnetCidrBlockStateCodeFailed
    , fromSubnetCidrBlockStateCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SubnetCidrBlockStateCode = SubnetCidrBlockStateCode'{fromSubnetCidrBlockStateCode
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern SubnetCidrBlockStateCodeAssociating :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCodeAssociating = SubnetCidrBlockStateCode' "associating"

pattern SubnetCidrBlockStateCodeAssociated :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCodeAssociated = SubnetCidrBlockStateCode' "associated"

pattern SubnetCidrBlockStateCodeDisassociating :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCodeDisassociating = SubnetCidrBlockStateCode' "disassociating"

pattern SubnetCidrBlockStateCodeDisassociated :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCodeDisassociated = SubnetCidrBlockStateCode' "disassociated"

pattern SubnetCidrBlockStateCodeFailing :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCodeFailing = SubnetCidrBlockStateCode' "failing"

pattern SubnetCidrBlockStateCodeFailed :: SubnetCidrBlockStateCode
pattern SubnetCidrBlockStateCodeFailed = SubnetCidrBlockStateCode' "failed"

{-# COMPLETE 
  SubnetCidrBlockStateCodeAssociating,

  SubnetCidrBlockStateCodeAssociated,

  SubnetCidrBlockStateCodeDisassociating,

  SubnetCidrBlockStateCodeDisassociated,

  SubnetCidrBlockStateCodeFailing,

  SubnetCidrBlockStateCodeFailed,
  SubnetCidrBlockStateCode'
  #-}
