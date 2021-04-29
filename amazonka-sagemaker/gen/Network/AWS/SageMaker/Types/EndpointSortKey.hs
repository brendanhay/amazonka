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

import qualified Network.AWS.Prelude as Prelude

newtype EndpointSortKey = EndpointSortKey'
  { fromEndpointSortKey ::
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
