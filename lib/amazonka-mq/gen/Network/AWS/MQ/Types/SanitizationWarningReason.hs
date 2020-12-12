{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.SanitizationWarningReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.SanitizationWarningReason
  ( SanitizationWarningReason
      ( SanitizationWarningReason',
        DisallowedAttributeRemoved,
        DisallowedElementRemoved,
        InvalidAttributeValueRemoved
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The reason for which the XML elements or attributes were sanitized.
newtype SanitizationWarningReason = SanitizationWarningReason' Lude.Text
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

pattern DisallowedAttributeRemoved :: SanitizationWarningReason
pattern DisallowedAttributeRemoved = SanitizationWarningReason' "DISALLOWED_ATTRIBUTE_REMOVED"

pattern DisallowedElementRemoved :: SanitizationWarningReason
pattern DisallowedElementRemoved = SanitizationWarningReason' "DISALLOWED_ELEMENT_REMOVED"

pattern InvalidAttributeValueRemoved :: SanitizationWarningReason
pattern InvalidAttributeValueRemoved = SanitizationWarningReason' "INVALID_ATTRIBUTE_VALUE_REMOVED"

{-# COMPLETE
  DisallowedAttributeRemoved,
  DisallowedElementRemoved,
  InvalidAttributeValueRemoved,
  SanitizationWarningReason'
  #-}
