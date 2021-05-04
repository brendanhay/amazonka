{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RIProductDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RIProductDescription
  ( RIProductDescription
      ( ..,
        RIProductDescription_Linux_UNIX,
        RIProductDescription_Linux_UNIX__Amazon_VPC_,
        RIProductDescription_Windows,
        RIProductDescription_Windows__Amazon_VPC_
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype RIProductDescription = RIProductDescription'
  { fromRIProductDescription ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
