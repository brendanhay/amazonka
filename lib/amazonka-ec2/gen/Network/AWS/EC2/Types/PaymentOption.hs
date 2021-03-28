{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PaymentOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.PaymentOption
  ( PaymentOption
    ( PaymentOption'
    , PaymentOptionAllUpfront
    , PaymentOptionPartialUpfront
    , PaymentOptionNoUpfront
    , fromPaymentOption
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PaymentOption = PaymentOption'{fromPaymentOption ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern PaymentOptionAllUpfront :: PaymentOption
pattern PaymentOptionAllUpfront = PaymentOption' "AllUpfront"

pattern PaymentOptionPartialUpfront :: PaymentOption
pattern PaymentOptionPartialUpfront = PaymentOption' "PartialUpfront"

pattern PaymentOptionNoUpfront :: PaymentOption
pattern PaymentOptionNoUpfront = PaymentOption' "NoUpfront"

{-# COMPLETE 
  PaymentOptionAllUpfront,

  PaymentOptionPartialUpfront,

  PaymentOptionNoUpfront,
  PaymentOption'
  #-}
