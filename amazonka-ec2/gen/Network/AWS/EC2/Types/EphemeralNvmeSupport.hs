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
-- Module      : Network.AWS.EC2.Types.EphemeralNvmeSupport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EphemeralNvmeSupport
  ( EphemeralNvmeSupport
      ( ..,
        EphemeralNvmeSupport_Required,
        EphemeralNvmeSupport_Supported,
        EphemeralNvmeSupport_Unsupported
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype EphemeralNvmeSupport = EphemeralNvmeSupport'
  { fromEphemeralNvmeSupport ::
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
