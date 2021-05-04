{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRuleTriggerType
  ( OrganizationConfigRuleTriggerType
      ( ..,
        OrganizationConfigRuleTriggerType_ConfigurationItemChangeNotification,
        OrganizationConfigRuleTriggerType_OversizedConfigurationItemChangeNotification,
        OrganizationConfigRuleTriggerType_ScheduledNotification
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OrganizationConfigRuleTriggerType = OrganizationConfigRuleTriggerType'
  { fromOrganizationConfigRuleTriggerType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern OrganizationConfigRuleTriggerType_ConfigurationItemChangeNotification :: OrganizationConfigRuleTriggerType
pattern OrganizationConfigRuleTriggerType_ConfigurationItemChangeNotification = OrganizationConfigRuleTriggerType' "ConfigurationItemChangeNotification"

pattern OrganizationConfigRuleTriggerType_OversizedConfigurationItemChangeNotification :: OrganizationConfigRuleTriggerType
pattern OrganizationConfigRuleTriggerType_OversizedConfigurationItemChangeNotification = OrganizationConfigRuleTriggerType' "OversizedConfigurationItemChangeNotification"

pattern OrganizationConfigRuleTriggerType_ScheduledNotification :: OrganizationConfigRuleTriggerType
pattern OrganizationConfigRuleTriggerType_ScheduledNotification = OrganizationConfigRuleTriggerType' "ScheduledNotification"

{-# COMPLETE
  OrganizationConfigRuleTriggerType_ConfigurationItemChangeNotification,
  OrganizationConfigRuleTriggerType_OversizedConfigurationItemChangeNotification,
  OrganizationConfigRuleTriggerType_ScheduledNotification,
  OrganizationConfigRuleTriggerType'
  #-}
