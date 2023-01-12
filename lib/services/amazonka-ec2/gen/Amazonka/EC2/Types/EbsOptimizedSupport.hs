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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype EbsOptimizedSupport = EbsOptimizedSupport'
  { fromEbsOptimizedSupport ::
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
