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
-- Module      : Amazonka.SecurityLake.Types.Region
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.Region
  ( Region
      ( ..,
        Region_Ap_northeast_1,
        Region_Ap_southeast_2,
        Region_Eu_central_1,
        Region_Eu_west_1,
        Region_Us_east_1,
        Region_Us_east_2,
        Region_Us_west_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Region = Region' {fromRegion :: Data.Text}
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

pattern Region_Ap_northeast_1 :: Region
pattern Region_Ap_northeast_1 = Region' "ap-northeast-1"

pattern Region_Ap_southeast_2 :: Region
pattern Region_Ap_southeast_2 = Region' "ap-southeast-2"

pattern Region_Eu_central_1 :: Region
pattern Region_Eu_central_1 = Region' "eu-central-1"

pattern Region_Eu_west_1 :: Region
pattern Region_Eu_west_1 = Region' "eu-west-1"

pattern Region_Us_east_1 :: Region
pattern Region_Us_east_1 = Region' "us-east-1"

pattern Region_Us_east_2 :: Region
pattern Region_Us_east_2 = Region' "us-east-2"

pattern Region_Us_west_2 :: Region
pattern Region_Us_west_2 = Region' "us-west-2"

{-# COMPLETE
  Region_Ap_northeast_1,
  Region_Ap_southeast_2,
  Region_Eu_central_1,
  Region_Eu_west_1,
  Region_Us_east_1,
  Region_Us_east_2,
  Region_Us_west_2,
  Region'
  #-}
