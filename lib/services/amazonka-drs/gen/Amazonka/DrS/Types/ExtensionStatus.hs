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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExtensionStatus = ExtensionStatus'
  { fromExtensionStatus ::
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
