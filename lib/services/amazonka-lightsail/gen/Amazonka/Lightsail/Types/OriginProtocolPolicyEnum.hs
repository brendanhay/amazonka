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
-- Module      : Amazonka.Lightsail.Types.OriginProtocolPolicyEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.OriginProtocolPolicyEnum
  ( OriginProtocolPolicyEnum
      ( ..,
        OriginProtocolPolicyEnum_Http_only,
        OriginProtocolPolicyEnum_Https_only
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OriginProtocolPolicyEnum = OriginProtocolPolicyEnum'
  { fromOriginProtocolPolicyEnum ::
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

pattern OriginProtocolPolicyEnum_Http_only :: OriginProtocolPolicyEnum
pattern OriginProtocolPolicyEnum_Http_only = OriginProtocolPolicyEnum' "http-only"

pattern OriginProtocolPolicyEnum_Https_only :: OriginProtocolPolicyEnum
pattern OriginProtocolPolicyEnum_Https_only = OriginProtocolPolicyEnum' "https-only"

{-# COMPLETE
  OriginProtocolPolicyEnum_Http_only,
  OriginProtocolPolicyEnum_Https_only,
  OriginProtocolPolicyEnum'
  #-}
