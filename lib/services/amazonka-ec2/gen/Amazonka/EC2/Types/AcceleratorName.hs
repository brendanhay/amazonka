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
-- Module      : Amazonka.EC2.Types.AcceleratorName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AcceleratorName
  ( AcceleratorName
      ( ..,
        AcceleratorName_A100,
        AcceleratorName_Inferentia,
        AcceleratorName_K520,
        AcceleratorName_K80,
        AcceleratorName_M60,
        AcceleratorName_Radeon_pro_v520,
        AcceleratorName_T4,
        AcceleratorName_V100,
        AcceleratorName_Vu9p
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype AcceleratorName = AcceleratorName'
  { fromAcceleratorName ::
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

pattern AcceleratorName_A100 :: AcceleratorName
pattern AcceleratorName_A100 = AcceleratorName' "a100"

pattern AcceleratorName_Inferentia :: AcceleratorName
pattern AcceleratorName_Inferentia = AcceleratorName' "inferentia"

pattern AcceleratorName_K520 :: AcceleratorName
pattern AcceleratorName_K520 = AcceleratorName' "k520"

pattern AcceleratorName_K80 :: AcceleratorName
pattern AcceleratorName_K80 = AcceleratorName' "k80"

pattern AcceleratorName_M60 :: AcceleratorName
pattern AcceleratorName_M60 = AcceleratorName' "m60"

pattern AcceleratorName_Radeon_pro_v520 :: AcceleratorName
pattern AcceleratorName_Radeon_pro_v520 = AcceleratorName' "radeon-pro-v520"

pattern AcceleratorName_T4 :: AcceleratorName
pattern AcceleratorName_T4 = AcceleratorName' "t4"

pattern AcceleratorName_V100 :: AcceleratorName
pattern AcceleratorName_V100 = AcceleratorName' "v100"

pattern AcceleratorName_Vu9p :: AcceleratorName
pattern AcceleratorName_Vu9p = AcceleratorName' "vu9p"

{-# COMPLETE
  AcceleratorName_A100,
  AcceleratorName_Inferentia,
  AcceleratorName_K520,
  AcceleratorName_K80,
  AcceleratorName_M60,
  AcceleratorName_Radeon_pro_v520,
  AcceleratorName_T4,
  AcceleratorName_V100,
  AcceleratorName_Vu9p,
  AcceleratorName'
  #-}
