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
-- Module      : Amazonka.CloudFormation.Types.ThirdPartyType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ThirdPartyType
  ( ThirdPartyType
      ( ..,
        ThirdPartyType_HOOK,
        ThirdPartyType_MODULE,
        ThirdPartyType_RESOURCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ThirdPartyType = ThirdPartyType'
  { fromThirdPartyType ::
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

pattern ThirdPartyType_HOOK :: ThirdPartyType
pattern ThirdPartyType_HOOK = ThirdPartyType' "HOOK"

pattern ThirdPartyType_MODULE :: ThirdPartyType
pattern ThirdPartyType_MODULE = ThirdPartyType' "MODULE"

pattern ThirdPartyType_RESOURCE :: ThirdPartyType
pattern ThirdPartyType_RESOURCE = ThirdPartyType' "RESOURCE"

{-# COMPLETE
  ThirdPartyType_HOOK,
  ThirdPartyType_MODULE,
  ThirdPartyType_RESOURCE,
  ThirdPartyType'
  #-}
