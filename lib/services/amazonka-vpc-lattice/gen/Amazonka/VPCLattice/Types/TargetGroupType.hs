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
-- Module      : Amazonka.VPCLattice.Types.TargetGroupType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetGroupType
  ( TargetGroupType
      ( ..,
        TargetGroupType_ALB,
        TargetGroupType_INSTANCE,
        TargetGroupType_IP,
        TargetGroupType_LAMBDA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetGroupType = TargetGroupType'
  { fromTargetGroupType ::
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

pattern TargetGroupType_ALB :: TargetGroupType
pattern TargetGroupType_ALB = TargetGroupType' "ALB"

pattern TargetGroupType_INSTANCE :: TargetGroupType
pattern TargetGroupType_INSTANCE = TargetGroupType' "INSTANCE"

pattern TargetGroupType_IP :: TargetGroupType
pattern TargetGroupType_IP = TargetGroupType' "IP"

pattern TargetGroupType_LAMBDA :: TargetGroupType
pattern TargetGroupType_LAMBDA = TargetGroupType' "LAMBDA"

{-# COMPLETE
  TargetGroupType_ALB,
  TargetGroupType_INSTANCE,
  TargetGroupType_IP,
  TargetGroupType_LAMBDA,
  TargetGroupType'
  #-}
