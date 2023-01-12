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
-- Module      : Amazonka.EC2.Types.EphemeralNvmeSupport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EphemeralNvmeSupport
  ( EphemeralNvmeSupport
      ( ..,
        EphemeralNvmeSupport_Required,
        EphemeralNvmeSupport_Supported,
        EphemeralNvmeSupport_Unsupported
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype EphemeralNvmeSupport = EphemeralNvmeSupport'
  { fromEphemeralNvmeSupport ::
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

pattern EphemeralNvmeSupport_Required :: EphemeralNvmeSupport
pattern EphemeralNvmeSupport_Required = EphemeralNvmeSupport' "required"

pattern EphemeralNvmeSupport_Supported :: EphemeralNvmeSupport
pattern EphemeralNvmeSupport_Supported = EphemeralNvmeSupport' "supported"

pattern EphemeralNvmeSupport_Unsupported :: EphemeralNvmeSupport
pattern EphemeralNvmeSupport_Unsupported = EphemeralNvmeSupport' "unsupported"

{-# COMPLETE
  EphemeralNvmeSupport_Required,
  EphemeralNvmeSupport_Supported,
  EphemeralNvmeSupport_Unsupported,
  EphemeralNvmeSupport'
  #-}
