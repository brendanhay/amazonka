{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
  ( DefaultEmailOptionType
    ( DefaultEmailOptionType'
    , DefaultEmailOptionTypeConfirmWithLink
    , DefaultEmailOptionTypeConfirmWithCode
    , fromDefaultEmailOptionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DefaultEmailOptionType = DefaultEmailOptionType'{fromDefaultEmailOptionType
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern DefaultEmailOptionTypeConfirmWithLink :: DefaultEmailOptionType
pattern DefaultEmailOptionTypeConfirmWithLink = DefaultEmailOptionType' "CONFIRM_WITH_LINK"

pattern DefaultEmailOptionTypeConfirmWithCode :: DefaultEmailOptionType
pattern DefaultEmailOptionTypeConfirmWithCode = DefaultEmailOptionType' "CONFIRM_WITH_CODE"

{-# COMPLETE 
  DefaultEmailOptionTypeConfirmWithLink,

  DefaultEmailOptionTypeConfirmWithCode,
  DefaultEmailOptionType'
  #-}
