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
-- Module      : Amazonka.DrS.Types.ExtensionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ExtensionStatus
  ( ExtensionStatus
      ( ..,
        ExtensionStatus_EXTENDED,
        ExtensionStatus_EXTENSION_ERROR,
        ExtensionStatus_NOT_EXTENDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ExtensionStatus = ExtensionStatus'
  { fromExtensionStatus ::
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

pattern ExtensionStatus_EXTENDED :: ExtensionStatus
pattern ExtensionStatus_EXTENDED = ExtensionStatus' "EXTENDED"

pattern ExtensionStatus_EXTENSION_ERROR :: ExtensionStatus
pattern ExtensionStatus_EXTENSION_ERROR = ExtensionStatus' "EXTENSION_ERROR"

pattern ExtensionStatus_NOT_EXTENDED :: ExtensionStatus
pattern ExtensionStatus_NOT_EXTENDED = ExtensionStatus' "NOT_EXTENDED"

{-# COMPLETE
  ExtensionStatus_EXTENDED,
  ExtensionStatus_EXTENSION_ERROR,
  ExtensionStatus_NOT_EXTENDED,
  ExtensionStatus'
  #-}
