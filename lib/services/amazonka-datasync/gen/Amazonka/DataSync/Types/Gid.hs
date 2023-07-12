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
-- Module      : Amazonka.DataSync.Types.Gid
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Gid
  ( Gid
      ( ..,
        Gid_BOTH,
        Gid_INT_VALUE,
        Gid_NAME,
        Gid_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Gid = Gid' {fromGid :: Data.Text}
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

pattern Gid_BOTH :: Gid
pattern Gid_BOTH = Gid' "BOTH"

pattern Gid_INT_VALUE :: Gid
pattern Gid_INT_VALUE = Gid' "INT_VALUE"

pattern Gid_NAME :: Gid
pattern Gid_NAME = Gid' "NAME"

pattern Gid_NONE :: Gid
pattern Gid_NONE = Gid' "NONE"

{-# COMPLETE
  Gid_BOTH,
  Gid_INT_VALUE,
  Gid_NAME,
  Gid_NONE,
  Gid'
  #-}
