{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
  ( InputDeviceConfiguredInput
      ( InputDeviceConfiguredInput',
        IDCIAuto,
        IDCIHdmi,
        IDCISdi
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The source to activate (use) from the input device.
newtype InputDeviceConfiguredInput = InputDeviceConfiguredInput' Lude.Text
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

pattern IDCIAuto :: InputDeviceConfiguredInput
pattern IDCIAuto = InputDeviceConfiguredInput' "AUTO"

pattern IDCIHdmi :: InputDeviceConfiguredInput
pattern IDCIHdmi = InputDeviceConfiguredInput' "HDMI"

pattern IDCISdi :: InputDeviceConfiguredInput
pattern IDCISdi = InputDeviceConfiguredInput' "SDI"

{-# COMPLETE
  IDCIAuto,
  IDCIHdmi,
  IDCISdi,
  InputDeviceConfiguredInput'
  #-}
