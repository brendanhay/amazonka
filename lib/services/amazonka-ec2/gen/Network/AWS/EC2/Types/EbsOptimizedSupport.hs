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
-- Module      : Amazonka.EC2.Types.EbsOptimizedSupport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EbsOptimizedSupport
  ( EbsOptimizedSupport
      ( ..,
        EbsOptimizedSupport_Default,
        EbsOptimizedSupport_Supported,
        EbsOptimizedSupport_Unsupported
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype EbsOptimizedSupport = EbsOptimizedSupport'
  { fromEbsOptimizedSupport ::
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

pattern EbsOptimizedSupport_Default :: EbsOptimizedSupport
pattern EbsOptimizedSupport_Default = EbsOptimizedSupport' "default"

pattern EbsOptimizedSupport_Supported :: EbsOptimizedSupport
pattern EbsOptimizedSupport_Supported = EbsOptimizedSupport' "supported"

pattern EbsOptimizedSupport_Unsupported :: EbsOptimizedSupport
pattern EbsOptimizedSupport_Unsupported = EbsOptimizedSupport' "unsupported"

{-# COMPLETE
  EbsOptimizedSupport_Default,
  EbsOptimizedSupport_Supported,
  EbsOptimizedSupport_Unsupported,
  EbsOptimizedSupport'
  #-}
