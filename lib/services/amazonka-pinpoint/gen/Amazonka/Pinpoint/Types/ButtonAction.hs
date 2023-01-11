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
-- Module      : Amazonka.Pinpoint.Types.ButtonAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ButtonAction
  ( ButtonAction
      ( ..,
        ButtonAction_CLOSE,
        ButtonAction_DEEP_LINK,
        ButtonAction_LINK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ButtonAction = ButtonAction'
  { fromButtonAction ::
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
