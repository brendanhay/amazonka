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
-- Module      : Amazonka.DMS.Types.TlogAccessMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.TlogAccessMode
  ( TlogAccessMode
      ( ..,
        TlogAccessMode_BackupOnly,
        TlogAccessMode_PreferBackup,
        TlogAccessMode_PreferTlog,
        TlogAccessMode_TlogOnly
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TlogAccessMode = TlogAccessMode'
  { fromTlogAccessMode ::
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

pattern TlogAccessMode_BackupOnly :: TlogAccessMode
pattern TlogAccessMode_BackupOnly = TlogAccessMode' "BackupOnly"

pattern TlogAccessMode_PreferBackup :: TlogAccessMode
pattern TlogAccessMode_PreferBackup = TlogAccessMode' "PreferBackup"

pattern TlogAccessMode_PreferTlog :: TlogAccessMode
pattern TlogAccessMode_PreferTlog = TlogAccessMode' "PreferTlog"

pattern TlogAccessMode_TlogOnly :: TlogAccessMode
pattern TlogAccessMode_TlogOnly = TlogAccessMode' "TlogOnly"

{-# COMPLETE
  TlogAccessMode_BackupOnly,
  TlogAccessMode_PreferBackup,
  TlogAccessMode_PreferTlog,
  TlogAccessMode_TlogOnly,
  TlogAccessMode'
  #-}
