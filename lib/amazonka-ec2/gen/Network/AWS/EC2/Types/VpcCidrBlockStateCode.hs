{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VpcCidrBlockStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VpcCidrBlockStateCode
  ( VpcCidrBlockStateCode
    ( VpcCidrBlockStateCode'
    , VpcCidrBlockStateCodeAssociating
    , VpcCidrBlockStateCodeAssociated
    , VpcCidrBlockStateCodeDisassociating
    , VpcCidrBlockStateCodeDisassociated
    , VpcCidrBlockStateCodeFailing
    , VpcCidrBlockStateCodeFailed
    , fromVpcCidrBlockStateCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype VpcCidrBlockStateCode = VpcCidrBlockStateCode'{fromVpcCidrBlockStateCode
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern VpcCidrBlockStateCodeAssociating :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCodeAssociating = VpcCidrBlockStateCode' "associating"

pattern VpcCidrBlockStateCodeAssociated :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCodeAssociated = VpcCidrBlockStateCode' "associated"

pattern VpcCidrBlockStateCodeDisassociating :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCodeDisassociating = VpcCidrBlockStateCode' "disassociating"

pattern VpcCidrBlockStateCodeDisassociated :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCodeDisassociated = VpcCidrBlockStateCode' "disassociated"

pattern VpcCidrBlockStateCodeFailing :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCodeFailing = VpcCidrBlockStateCode' "failing"

pattern VpcCidrBlockStateCodeFailed :: VpcCidrBlockStateCode
pattern VpcCidrBlockStateCodeFailed = VpcCidrBlockStateCode' "failed"

{-# COMPLETE 
  VpcCidrBlockStateCodeAssociating,

  VpcCidrBlockStateCodeAssociated,

  VpcCidrBlockStateCodeDisassociating,

  VpcCidrBlockStateCodeDisassociated,

  VpcCidrBlockStateCodeFailing,

  VpcCidrBlockStateCodeFailed,
  VpcCidrBlockStateCode'
  #-}
