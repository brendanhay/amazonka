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
-- Module      : Network.AWS.MediaConvert.Types.DescribeEndpointsMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DescribeEndpointsMode
  ( DescribeEndpointsMode
      ( ..,
        DescribeEndpointsMode_DEFAULT,
        DescribeEndpointsMode_GET_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
-- to return your endpoints if any exist, or to create an endpoint for you
-- and return it if one doesn\'t already exist. Specify GET_ONLY to return
-- your endpoints if any exist, or an empty list if none exist.
newtype DescribeEndpointsMode = DescribeEndpointsMode'
  { fromDescribeEndpointsMode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DescribeEndpointsMode_DEFAULT :: DescribeEndpointsMode
pattern DescribeEndpointsMode_DEFAULT = DescribeEndpointsMode' "DEFAULT"

pattern DescribeEndpointsMode_GET_ONLY :: DescribeEndpointsMode
pattern DescribeEndpointsMode_GET_ONLY = DescribeEndpointsMode' "GET_ONLY"

{-# COMPLETE
  DescribeEndpointsMode_DEFAULT,
  DescribeEndpointsMode_GET_ONLY,
  DescribeEndpointsMode'
  #-}
