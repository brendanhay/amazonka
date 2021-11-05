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
-- Module      : Amazonka.Snowball.Types.TransferOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.TransferOption
  ( TransferOption
      ( ..,
        TransferOption_EXPORT,
        TransferOption_IMPORT,
        TransferOption_LOCAL_USE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TransferOption = TransferOption'
  { fromTransferOption ::
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

pattern TransferOption_EXPORT :: TransferOption
pattern TransferOption_EXPORT = TransferOption' "EXPORT"

pattern TransferOption_IMPORT :: TransferOption
pattern TransferOption_IMPORT = TransferOption' "IMPORT"

pattern TransferOption_LOCAL_USE :: TransferOption
pattern TransferOption_LOCAL_USE = TransferOption' "LOCAL_USE"

{-# COMPLETE
  TransferOption_EXPORT,
  TransferOption_IMPORT,
  TransferOption_LOCAL_USE,
  TransferOption'
  #-}
