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
-- Module      : Network.AWS.MediaLive.Types.M2tsPcrControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsPcrControl
  ( M2tsPcrControl
      ( ..,
        M2tsPcrControl_CONFIGURED_PCR_PERIOD,
        M2tsPcrControl_PCR_EVERY_PES_PACKET
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | M2ts Pcr Control
newtype M2tsPcrControl = M2tsPcrControl'
  { fromM2tsPcrControl ::
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

pattern M2tsPcrControl_CONFIGURED_PCR_PERIOD :: M2tsPcrControl
pattern M2tsPcrControl_CONFIGURED_PCR_PERIOD = M2tsPcrControl' "CONFIGURED_PCR_PERIOD"

pattern M2tsPcrControl_PCR_EVERY_PES_PACKET :: M2tsPcrControl
pattern M2tsPcrControl_PCR_EVERY_PES_PACKET = M2tsPcrControl' "PCR_EVERY_PES_PACKET"

{-# COMPLETE
  M2tsPcrControl_CONFIGURED_PCR_PERIOD,
  M2tsPcrControl_PCR_EVERY_PES_PACKET,
  M2tsPcrControl'
  #-}
