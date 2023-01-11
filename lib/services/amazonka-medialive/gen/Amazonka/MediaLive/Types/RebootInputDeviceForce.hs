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
-- Module      : Amazonka.MediaLive.Types.RebootInputDeviceForce
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RebootInputDeviceForce
  ( RebootInputDeviceForce
      ( ..,
        RebootInputDeviceForce_NO,
        RebootInputDeviceForce_YES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Whether or not to force reboot the input device.
newtype RebootInputDeviceForce = RebootInputDeviceForce'
  { fromRebootInputDeviceForce ::
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

pattern RebootInputDeviceForce_NO :: RebootInputDeviceForce
pattern RebootInputDeviceForce_NO = RebootInputDeviceForce' "NO"

pattern RebootInputDeviceForce_YES :: RebootInputDeviceForce
pattern RebootInputDeviceForce_YES = RebootInputDeviceForce' "YES"

{-# COMPLETE
  RebootInputDeviceForce_NO,
  RebootInputDeviceForce_YES,
  RebootInputDeviceForce'
  #-}
