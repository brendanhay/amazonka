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
-- Module      : Amazonka.EC2.Types.RIProductDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RIProductDescription
  ( RIProductDescription
      ( ..,
        RIProductDescription_Linux_UNIX,
        RIProductDescription_Linux_UNIX__Amazon_VPC_,
        RIProductDescription_Windows,
        RIProductDescription_Windows__Amazon_VPC_
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype RIProductDescription = RIProductDescription'
  { fromRIProductDescription ::
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

pattern RIProductDescription_Linux_UNIX :: RIProductDescription
pattern RIProductDescription_Linux_UNIX = RIProductDescription' "Linux/UNIX"

pattern RIProductDescription_Linux_UNIX__Amazon_VPC_ :: RIProductDescription
pattern RIProductDescription_Linux_UNIX__Amazon_VPC_ = RIProductDescription' "Linux/UNIX (Amazon VPC)"

pattern RIProductDescription_Windows :: RIProductDescription
pattern RIProductDescription_Windows = RIProductDescription' "Windows"

pattern RIProductDescription_Windows__Amazon_VPC_ :: RIProductDescription
pattern RIProductDescription_Windows__Amazon_VPC_ = RIProductDescription' "Windows (Amazon VPC)"

{-# COMPLETE
  RIProductDescription_Linux_UNIX,
  RIProductDescription_Linux_UNIX__Amazon_VPC_,
  RIProductDescription_Windows,
  RIProductDescription_Windows__Amazon_VPC_,
  RIProductDescription'
  #-}
