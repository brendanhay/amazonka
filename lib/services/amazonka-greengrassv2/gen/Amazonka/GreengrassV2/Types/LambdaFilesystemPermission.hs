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
-- Module      : Amazonka.GreengrassV2.Types.LambdaFilesystemPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaFilesystemPermission
  ( LambdaFilesystemPermission
      ( ..,
        LambdaFilesystemPermission_Ro,
        LambdaFilesystemPermission_Rw
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LambdaFilesystemPermission = LambdaFilesystemPermission'
  { fromLambdaFilesystemPermission ::
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

pattern LambdaFilesystemPermission_Ro :: LambdaFilesystemPermission
pattern LambdaFilesystemPermission_Ro = LambdaFilesystemPermission' "ro"

pattern LambdaFilesystemPermission_Rw :: LambdaFilesystemPermission
pattern LambdaFilesystemPermission_Rw = LambdaFilesystemPermission' "rw"

{-# COMPLETE
  LambdaFilesystemPermission_Ro,
  LambdaFilesystemPermission_Rw,
  LambdaFilesystemPermission'
  #-}
