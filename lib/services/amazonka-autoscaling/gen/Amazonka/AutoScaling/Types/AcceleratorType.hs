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
-- Module      : Amazonka.AutoScaling.Types.AcceleratorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.AcceleratorType
  ( AcceleratorType
      ( ..,
        AcceleratorType_Fpga,
        AcceleratorType_Gpu,
        AcceleratorType_Inference
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AcceleratorType = AcceleratorType'
  { fromAcceleratorType ::
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

pattern AcceleratorType_Fpga :: AcceleratorType
pattern AcceleratorType_Fpga = AcceleratorType' "fpga"

pattern AcceleratorType_Gpu :: AcceleratorType
pattern AcceleratorType_Gpu = AcceleratorType' "gpu"

pattern AcceleratorType_Inference :: AcceleratorType
pattern AcceleratorType_Inference = AcceleratorType' "inference"

{-# COMPLETE
  AcceleratorType_Fpga,
  AcceleratorType_Gpu,
  AcceleratorType_Inference,
  AcceleratorType'
  #-}
