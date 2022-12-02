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
-- Module      : Amazonka.CodeDeploy.Types.EC2TagFilterType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.EC2TagFilterType
  ( EC2TagFilterType
      ( ..,
        EC2TagFilterType_KEY_AND_VALUE,
        EC2TagFilterType_KEY_ONLY,
        EC2TagFilterType_VALUE_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EC2TagFilterType = EC2TagFilterType'
  { fromEC2TagFilterType ::
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

pattern EC2TagFilterType_KEY_AND_VALUE :: EC2TagFilterType
pattern EC2TagFilterType_KEY_AND_VALUE = EC2TagFilterType' "KEY_AND_VALUE"

pattern EC2TagFilterType_KEY_ONLY :: EC2TagFilterType
pattern EC2TagFilterType_KEY_ONLY = EC2TagFilterType' "KEY_ONLY"

pattern EC2TagFilterType_VALUE_ONLY :: EC2TagFilterType
pattern EC2TagFilterType_VALUE_ONLY = EC2TagFilterType' "VALUE_ONLY"

{-# COMPLETE
  EC2TagFilterType_KEY_AND_VALUE,
  EC2TagFilterType_KEY_ONLY,
  EC2TagFilterType_VALUE_ONLY,
  EC2TagFilterType'
  #-}
