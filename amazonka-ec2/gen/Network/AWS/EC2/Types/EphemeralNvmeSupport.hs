{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype EphemeralNvmeSupport = EphemeralNvmeSupport'
  { fromEphemeralNvmeSupport ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
