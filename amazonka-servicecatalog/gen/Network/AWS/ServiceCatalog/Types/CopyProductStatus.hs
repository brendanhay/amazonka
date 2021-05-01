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
-- Module      : Network.AWS.ServiceCatalog.Types.CopyProductStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.CopyProductStatus
  ( CopyProductStatus
      ( ..,
        CopyProductStatus_FAILED,
        CopyProductStatus_IN_PROGRESS,
        CopyProductStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CopyProductStatus = CopyProductStatus'
  { fromCopyProductStatus ::
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

pattern CopyProductStatus_FAILED :: CopyProductStatus
pattern CopyProductStatus_FAILED = CopyProductStatus' "FAILED"

pattern CopyProductStatus_IN_PROGRESS :: CopyProductStatus
pattern CopyProductStatus_IN_PROGRESS = CopyProductStatus' "IN_PROGRESS"

pattern CopyProductStatus_SUCCEEDED :: CopyProductStatus
pattern CopyProductStatus_SUCCEEDED = CopyProductStatus' "SUCCEEDED"

{-# COMPLETE
  CopyProductStatus_FAILED,
  CopyProductStatus_IN_PROGRESS,
  CopyProductStatus_SUCCEEDED,
  CopyProductStatus'
  #-}
