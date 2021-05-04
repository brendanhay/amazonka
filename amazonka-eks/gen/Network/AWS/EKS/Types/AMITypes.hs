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
-- Module      : Network.AWS.EKS.Types.AMITypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.AMITypes
  ( AMITypes
      ( ..,
        AMITypes_AL2_ARM_64,
        AMITypes_AL2_x86_64,
        AMITypes_AL2_x86_64_GPU
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AMITypes = AMITypes'
  { fromAMITypes ::
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

pattern AMITypes_AL2_ARM_64 :: AMITypes
pattern AMITypes_AL2_ARM_64 = AMITypes' "AL2_ARM_64"

pattern AMITypes_AL2_x86_64 :: AMITypes
pattern AMITypes_AL2_x86_64 = AMITypes' "AL2_x86_64"

pattern AMITypes_AL2_x86_64_GPU :: AMITypes
pattern AMITypes_AL2_x86_64_GPU = AMITypes' "AL2_x86_64_GPU"

{-# COMPLETE
  AMITypes_AL2_ARM_64,
  AMITypes_AL2_x86_64,
  AMITypes_AL2_x86_64_GPU,
  AMITypes'
  #-}
