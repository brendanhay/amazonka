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
-- Module      : Amazonka.MediaConvert.Types.CopyProtectionAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CopyProtectionAction
  ( CopyProtectionAction
      ( ..,
        CopyProtectionAction_PASSTHROUGH,
        CopyProtectionAction_STRIP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The action to take on copy and redistribution control XDS packets. If
-- you select PASSTHROUGH, packets will not be changed. If you select
-- STRIP, any packets will be removed in output captions.
newtype CopyProtectionAction = CopyProtectionAction'
  { fromCopyProtectionAction ::
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

pattern CopyProtectionAction_PASSTHROUGH :: CopyProtectionAction
pattern CopyProtectionAction_PASSTHROUGH = CopyProtectionAction' "PASSTHROUGH"

pattern CopyProtectionAction_STRIP :: CopyProtectionAction
pattern CopyProtectionAction_STRIP = CopyProtectionAction' "STRIP"

{-# COMPLETE
  CopyProtectionAction_PASSTHROUGH,
  CopyProtectionAction_STRIP,
  CopyProtectionAction'
  #-}
