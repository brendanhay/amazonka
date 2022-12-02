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
-- Module      : Amazonka.EC2.Types.VpnEcmpSupportValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpnEcmpSupportValue
  ( VpnEcmpSupportValue
      ( ..,
        VpnEcmpSupportValue_Disable,
        VpnEcmpSupportValue_Enable
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype VpnEcmpSupportValue = VpnEcmpSupportValue'
  { fromVpnEcmpSupportValue ::
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

pattern VpnEcmpSupportValue_Disable :: VpnEcmpSupportValue
pattern VpnEcmpSupportValue_Disable = VpnEcmpSupportValue' "disable"

pattern VpnEcmpSupportValue_Enable :: VpnEcmpSupportValue
pattern VpnEcmpSupportValue_Enable = VpnEcmpSupportValue' "enable"

{-# COMPLETE
  VpnEcmpSupportValue_Disable,
  VpnEcmpSupportValue_Enable,
  VpnEcmpSupportValue'
  #-}
