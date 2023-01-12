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
-- Module      : Amazonka.DataSync.Types.NfsVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.NfsVersion
  ( NfsVersion
      ( ..,
        NfsVersion_AUTOMATIC,
        NfsVersion_NFS3,
        NfsVersion_NFS4_0,
        NfsVersion_NFS4_1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NfsVersion = NfsVersion'
  { fromNfsVersion ::
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

pattern NfsVersion_AUTOMATIC :: NfsVersion
pattern NfsVersion_AUTOMATIC = NfsVersion' "AUTOMATIC"

pattern NfsVersion_NFS3 :: NfsVersion
pattern NfsVersion_NFS3 = NfsVersion' "NFS3"

pattern NfsVersion_NFS4_0 :: NfsVersion
pattern NfsVersion_NFS4_0 = NfsVersion' "NFS4_0"

pattern NfsVersion_NFS4_1 :: NfsVersion
pattern NfsVersion_NFS4_1 = NfsVersion' "NFS4_1"

{-# COMPLETE
  NfsVersion_AUTOMATIC,
  NfsVersion_NFS3,
  NfsVersion_NFS4_0,
  NfsVersion_NFS4_1,
  NfsVersion'
  #-}
