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
-- Module      : Amazonka.Inspector2.Types.Ec2Platform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Ec2Platform
  ( Ec2Platform
      ( ..,
        Ec2Platform_LINUX,
        Ec2Platform_UNKNOWN,
        Ec2Platform_WINDOWS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Ec2Platform = Ec2Platform'
  { fromEc2Platform ::
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

pattern Ec2Platform_LINUX :: Ec2Platform
pattern Ec2Platform_LINUX = Ec2Platform' "LINUX"

pattern Ec2Platform_UNKNOWN :: Ec2Platform
pattern Ec2Platform_UNKNOWN = Ec2Platform' "UNKNOWN"

pattern Ec2Platform_WINDOWS :: Ec2Platform
pattern Ec2Platform_WINDOWS = Ec2Platform' "WINDOWS"

{-# COMPLETE
  Ec2Platform_LINUX,
  Ec2Platform_UNKNOWN,
  Ec2Platform_WINDOWS,
  Ec2Platform'
  #-}
