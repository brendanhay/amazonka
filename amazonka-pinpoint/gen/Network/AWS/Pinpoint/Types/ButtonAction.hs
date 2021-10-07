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
-- Module      : Network.AWS.Pinpoint.Types.ButtonAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ButtonAction
  ( ButtonAction
      ( ..,
        ButtonAction_CLOSE,
        ButtonAction_DEEP_LINK,
        ButtonAction_LINK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ButtonAction = ButtonAction'
  { fromButtonAction ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ButtonAction_CLOSE :: ButtonAction
pattern ButtonAction_CLOSE = ButtonAction' "CLOSE"

pattern ButtonAction_DEEP_LINK :: ButtonAction
pattern ButtonAction_DEEP_LINK = ButtonAction' "DEEP_LINK"

pattern ButtonAction_LINK :: ButtonAction
pattern ButtonAction_LINK = ButtonAction' "LINK"

{-# COMPLETE
  ButtonAction_CLOSE,
  ButtonAction_DEEP_LINK,
  ButtonAction_LINK,
  ButtonAction'
  #-}
