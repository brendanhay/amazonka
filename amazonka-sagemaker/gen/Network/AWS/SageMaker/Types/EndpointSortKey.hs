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
-- Module      : Network.AWS.SageMaker.Types.EndpointSortKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointSortKey
  ( EndpointSortKey
      ( ..,
        EndpointSortKey_CreationTime,
        EndpointSortKey_Name,
        EndpointSortKey_Status
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EndpointSortKey = EndpointSortKey'
  { fromEndpointSortKey ::
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

pattern EndpointSortKey_CreationTime :: EndpointSortKey
pattern EndpointSortKey_CreationTime = EndpointSortKey' "CreationTime"

pattern EndpointSortKey_Name :: EndpointSortKey
pattern EndpointSortKey_Name = EndpointSortKey' "Name"

pattern EndpointSortKey_Status :: EndpointSortKey
pattern EndpointSortKey_Status = EndpointSortKey' "Status"

{-# COMPLETE
  EndpointSortKey_CreationTime,
  EndpointSortKey_Name,
  EndpointSortKey_Status,
  EndpointSortKey'
  #-}
