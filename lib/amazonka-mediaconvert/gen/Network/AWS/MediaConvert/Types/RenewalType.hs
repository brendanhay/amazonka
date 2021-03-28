{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.RenewalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.RenewalType
  ( RenewalType
    ( RenewalType'
    , RenewalTypeAutoRenew
    , RenewalTypeExpire
    , fromRenewalType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
newtype RenewalType = RenewalType'{fromRenewalType :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern RenewalTypeAutoRenew :: RenewalType
pattern RenewalTypeAutoRenew = RenewalType' "AUTO_RENEW"

pattern RenewalTypeExpire :: RenewalType
pattern RenewalTypeExpire = RenewalType' "EXPIRE"

{-# COMPLETE 
  RenewalTypeAutoRenew,

  RenewalTypeExpire,
  RenewalType'
  #-}
