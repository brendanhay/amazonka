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
-- Module      : Network.AWS.CloudFront.Types.OriginProtocolPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginProtocolPolicy
  ( OriginProtocolPolicy
      ( ..,
        OriginProtocolPolicy_Http_only,
        OriginProtocolPolicy_Https_only,
        OriginProtocolPolicy_Match_viewer
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype OriginProtocolPolicy = OriginProtocolPolicy'
  { fromOriginProtocolPolicy ::
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
