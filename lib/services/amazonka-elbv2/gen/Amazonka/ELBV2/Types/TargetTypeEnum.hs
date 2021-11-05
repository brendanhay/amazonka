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
-- Module      : Amazonka.ELBV2.Types.TargetTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TargetTypeEnum
  ( TargetTypeEnum
      ( ..,
        TargetTypeEnum_Alb,
        TargetTypeEnum_Instance,
        TargetTypeEnum_Ip,
        TargetTypeEnum_Lambda
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TargetTypeEnum = TargetTypeEnum'
  { fromTargetTypeEnum ::
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

pattern TargetTypeEnum_Alb :: TargetTypeEnum
pattern TargetTypeEnum_Alb = TargetTypeEnum' "alb"

pattern TargetTypeEnum_Instance :: TargetTypeEnum
pattern TargetTypeEnum_Instance = TargetTypeEnum' "instance"

pattern TargetTypeEnum_Ip :: TargetTypeEnum
pattern TargetTypeEnum_Ip = TargetTypeEnum' "ip"

pattern TargetTypeEnum_Lambda :: TargetTypeEnum
pattern TargetTypeEnum_Lambda = TargetTypeEnum' "lambda"

{-# COMPLETE
  TargetTypeEnum_Alb,
  TargetTypeEnum_Instance,
  TargetTypeEnum_Ip,
  TargetTypeEnum_Lambda,
  TargetTypeEnum'
  #-}
