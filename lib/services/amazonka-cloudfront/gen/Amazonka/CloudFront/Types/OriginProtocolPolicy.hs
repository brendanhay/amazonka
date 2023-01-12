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
-- Module      : Amazonka.CloudFront.Types.OriginProtocolPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginProtocolPolicy
  ( OriginProtocolPolicy
      ( ..,
        OriginProtocolPolicy_Http_only,
        OriginProtocolPolicy_Https_only,
        OriginProtocolPolicy_Match_viewer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OriginProtocolPolicy = OriginProtocolPolicy'
  { fromOriginProtocolPolicy ::
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

pattern OriginProtocolPolicy_Http_only :: OriginProtocolPolicy
pattern OriginProtocolPolicy_Http_only = OriginProtocolPolicy' "http-only"

pattern OriginProtocolPolicy_Https_only :: OriginProtocolPolicy
pattern OriginProtocolPolicy_Https_only = OriginProtocolPolicy' "https-only"

pattern OriginProtocolPolicy_Match_viewer :: OriginProtocolPolicy
pattern OriginProtocolPolicy_Match_viewer = OriginProtocolPolicy' "match-viewer"

{-# COMPLETE
  OriginProtocolPolicy_Http_only,
  OriginProtocolPolicy_Https_only,
  OriginProtocolPolicy_Match_viewer,
  OriginProtocolPolicy'
  #-}
