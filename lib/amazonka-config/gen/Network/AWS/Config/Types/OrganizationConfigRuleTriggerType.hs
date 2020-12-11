-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
  ( OrganizationConfigRuleTriggerType
      ( OrganizationConfigRuleTriggerType',
        OCRTTConfigurationItemChangeNotification,
        OCRTTOversizedConfigurationItemChangeNotification,
        OCRTTScheduledNotification
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OrganizationConfigRuleTriggerType = OrganizationConfigRuleTriggerType' Lude.Text
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

pattern OCRTTConfigurationItemChangeNotification :: OrganizationConfigRuleTriggerType
pattern OCRTTConfigurationItemChangeNotification = OrganizationConfigRuleTriggerType' "ConfigurationItemChangeNotification"

pattern OCRTTOversizedConfigurationItemChangeNotification :: OrganizationConfigRuleTriggerType
pattern OCRTTOversizedConfigurationItemChangeNotification = OrganizationConfigRuleTriggerType' "OversizedConfigurationItemChangeNotification"

pattern OCRTTScheduledNotification :: OrganizationConfigRuleTriggerType
pattern OCRTTScheduledNotification = OrganizationConfigRuleTriggerType' "ScheduledNotification"

{-# COMPLETE
  OCRTTConfigurationItemChangeNotification,
  OCRTTOversizedConfigurationItemChangeNotification,
  OCRTTScheduledNotification,
  OrganizationConfigRuleTriggerType'
  #-}
