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
-- Module      : Amazonka.AppMesh.Types.IpPreference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.IpPreference
  ( IpPreference
      ( ..,
        IpPreference_IPv4_ONLY,
        IpPreference_IPv4_PREFERRED,
        IpPreference_IPv6_ONLY,
        IpPreference_IPv6_PREFERRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IpPreference = IpPreference'
  { fromIpPreference ::
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

pattern IpPreference_IPv4_ONLY :: IpPreference
pattern IpPreference_IPv4_ONLY = IpPreference' "IPv4_ONLY"

pattern IpPreference_IPv4_PREFERRED :: IpPreference
pattern IpPreference_IPv4_PREFERRED = IpPreference' "IPv4_PREFERRED"

pattern IpPreference_IPv6_ONLY :: IpPreference
pattern IpPreference_IPv6_ONLY = IpPreference' "IPv6_ONLY"

pattern IpPreference_IPv6_PREFERRED :: IpPreference
pattern IpPreference_IPv6_PREFERRED = IpPreference' "IPv6_PREFERRED"

{-# COMPLETE
  IpPreference_IPv4_ONLY,
  IpPreference_IPv4_PREFERRED,
  IpPreference_IPv6_ONLY,
  IpPreference_IPv6_PREFERRED,
  IpPreference'
  #-}
