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
-- Module      : Amazonka.Snowball.Types.RemoteManagement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.RemoteManagement
  ( RemoteManagement
      ( ..,
        RemoteManagement_INSTALLED_AUTOSTART,
        RemoteManagement_INSTALLED_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RemoteManagement = RemoteManagement'
  { fromRemoteManagement ::
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

pattern RemoteManagement_INSTALLED_AUTOSTART :: RemoteManagement
pattern RemoteManagement_INSTALLED_AUTOSTART = RemoteManagement' "INSTALLED_AUTOSTART"

pattern RemoteManagement_INSTALLED_ONLY :: RemoteManagement
pattern RemoteManagement_INSTALLED_ONLY = RemoteManagement' "INSTALLED_ONLY"

{-# COMPLETE
  RemoteManagement_INSTALLED_AUTOSTART,
  RemoteManagement_INSTALLED_ONLY,
  RemoteManagement'
  #-}
