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
-- Module      : Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
  ( UnlimitedSupportedInstanceFamily
      ( ..,
        UnlimitedSupportedInstanceFamily_T2,
        UnlimitedSupportedInstanceFamily_T3,
        UnlimitedSupportedInstanceFamily_T3a,
        UnlimitedSupportedInstanceFamily_T4g
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype UnlimitedSupportedInstanceFamily = UnlimitedSupportedInstanceFamily'
  { fromUnlimitedSupportedInstanceFamily ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern UnlimitedSupportedInstanceFamily_T2 :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamily_T2 = UnlimitedSupportedInstanceFamily' "t2"

pattern UnlimitedSupportedInstanceFamily_T3 :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamily_T3 = UnlimitedSupportedInstanceFamily' "t3"

pattern UnlimitedSupportedInstanceFamily_T3a :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamily_T3a = UnlimitedSupportedInstanceFamily' "t3a"

pattern UnlimitedSupportedInstanceFamily_T4g :: UnlimitedSupportedInstanceFamily
pattern UnlimitedSupportedInstanceFamily_T4g = UnlimitedSupportedInstanceFamily' "t4g"

{-# COMPLETE
  UnlimitedSupportedInstanceFamily_T2,
  UnlimitedSupportedInstanceFamily_T3,
  UnlimitedSupportedInstanceFamily_T3a,
  UnlimitedSupportedInstanceFamily_T4g,
  UnlimitedSupportedInstanceFamily'
  #-}
