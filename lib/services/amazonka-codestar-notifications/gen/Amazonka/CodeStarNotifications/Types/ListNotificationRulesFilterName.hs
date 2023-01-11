{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeStarNotifications.Types.ListNotificationRulesFilterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.ListNotificationRulesFilterName
  ( ListNotificationRulesFilterName
      ( ..,
        ListNotificationRulesFilterName_CREATED_BY,
        ListNotificationRulesFilterName_EVENT_TYPE_ID,
        ListNotificationRulesFilterName_RESOURCE,
        ListNotificationRulesFilterName_TARGET_ADDRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListNotificationRulesFilterName = ListNotificationRulesFilterName'
  { fromListNotificationRulesFilterName ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ListNotificationRulesFilterName_CREATED_BY :: ListNotificationRulesFilterName
pattern ListNotificationRulesFilterName_CREATED_BY = ListNotificationRulesFilterName' "CREATED_BY"

pattern ListNotificationRulesFilterName_EVENT_TYPE_ID :: ListNotificationRulesFilterName
pattern ListNotificationRulesFilterName_EVENT_TYPE_ID = ListNotificationRulesFilterName' "EVENT_TYPE_ID"

pattern ListNotificationRulesFilterName_RESOURCE :: ListNotificationRulesFilterName
pattern ListNotificationRulesFilterName_RESOURCE = ListNotificationRulesFilterName' "RESOURCE"

pattern ListNotificationRulesFilterName_TARGET_ADDRESS :: ListNotificationRulesFilterName
pattern ListNotificationRulesFilterName_TARGET_ADDRESS = ListNotificationRulesFilterName' "TARGET_ADDRESS"

{-# COMPLETE
  ListNotificationRulesFilterName_CREATED_BY,
  ListNotificationRulesFilterName_EVENT_TYPE_ID,
  ListNotificationRulesFilterName_RESOURCE,
  ListNotificationRulesFilterName_TARGET_ADDRESS,
  ListNotificationRulesFilterName'
  #-}
