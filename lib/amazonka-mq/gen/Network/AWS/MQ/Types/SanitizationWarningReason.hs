{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.SanitizationWarningReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.SanitizationWarningReason
  ( SanitizationWarningReason
    ( SanitizationWarningReason'
    , SanitizationWarningReasonDisallowedElementRemoved
    , SanitizationWarningReasonDisallowedAttributeRemoved
    , SanitizationWarningReasonInvalidAttributeValueRemoved
    , fromSanitizationWarningReason
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The reason for which the XML elements or attributes were sanitized.
newtype SanitizationWarningReason = SanitizationWarningReason'{fromSanitizationWarningReason
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern SanitizationWarningReasonDisallowedElementRemoved :: SanitizationWarningReason
pattern SanitizationWarningReasonDisallowedElementRemoved = SanitizationWarningReason' "DISALLOWED_ELEMENT_REMOVED"

pattern SanitizationWarningReasonDisallowedAttributeRemoved :: SanitizationWarningReason
pattern SanitizationWarningReasonDisallowedAttributeRemoved = SanitizationWarningReason' "DISALLOWED_ATTRIBUTE_REMOVED"

pattern SanitizationWarningReasonInvalidAttributeValueRemoved :: SanitizationWarningReason
pattern SanitizationWarningReasonInvalidAttributeValueRemoved = SanitizationWarningReason' "INVALID_ATTRIBUTE_VALUE_REMOVED"

{-# COMPLETE 
  SanitizationWarningReasonDisallowedElementRemoved,

  SanitizationWarningReasonDisallowedAttributeRemoved,

  SanitizationWarningReasonInvalidAttributeValueRemoved,
  SanitizationWarningReason'
  #-}
