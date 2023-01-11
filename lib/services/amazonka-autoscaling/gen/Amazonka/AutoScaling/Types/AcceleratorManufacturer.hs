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
-- Module      : Amazonka.AutoScaling.Types.AcceleratorManufacturer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.AcceleratorManufacturer
  ( AcceleratorManufacturer
      ( ..,
        AcceleratorManufacturer_Amazon_web_services,
        AcceleratorManufacturer_Amd,
        AcceleratorManufacturer_Nvidia,
        AcceleratorManufacturer_Xilinx
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AcceleratorManufacturer = AcceleratorManufacturer'
  { fromAcceleratorManufacturer ::
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

pattern AcceleratorManufacturer_Amazon_web_services :: AcceleratorManufacturer
pattern AcceleratorManufacturer_Amazon_web_services = AcceleratorManufacturer' "amazon-web-services"

pattern AcceleratorManufacturer_Amd :: AcceleratorManufacturer
pattern AcceleratorManufacturer_Amd = AcceleratorManufacturer' "amd"

pattern AcceleratorManufacturer_Nvidia :: AcceleratorManufacturer
pattern AcceleratorManufacturer_Nvidia = AcceleratorManufacturer' "nvidia"

pattern AcceleratorManufacturer_Xilinx :: AcceleratorManufacturer
pattern AcceleratorManufacturer_Xilinx = AcceleratorManufacturer' "xilinx"

{-# COMPLETE
  AcceleratorManufacturer_Amazon_web_services,
  AcceleratorManufacturer_Amd,
  AcceleratorManufacturer_Nvidia,
  AcceleratorManufacturer_Xilinx,
  AcceleratorManufacturer'
  #-}
