{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AutoRegistrationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AutoRegistrationStatus
  ( AutoRegistrationStatus
      ( AutoRegistrationStatus',
        AutoRegistrationStatusEnable,
        AutoRegistrationStatusDisable,
        fromAutoRegistrationStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AutoRegistrationStatus = AutoRegistrationStatus'
  { fromAutoRegistrationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AutoRegistrationStatusEnable :: AutoRegistrationStatus
pattern AutoRegistrationStatusEnable = AutoRegistrationStatus' "ENABLE"

pattern AutoRegistrationStatusDisable :: AutoRegistrationStatus
pattern AutoRegistrationStatusDisable = AutoRegistrationStatus' "DISABLE"

{-# COMPLETE
  AutoRegistrationStatusEnable,
  AutoRegistrationStatusDisable,
  AutoRegistrationStatus'
  #-}
