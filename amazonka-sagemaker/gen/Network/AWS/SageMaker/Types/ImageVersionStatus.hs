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
-- Module      : Network.AWS.SageMaker.Types.ImageVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionStatus
  ( ImageVersionStatus
      ( ..,
        ImageVersionStatus_CREATED,
        ImageVersionStatus_CREATE_FAILED,
        ImageVersionStatus_CREATING,
        ImageVersionStatus_DELETE_FAILED,
        ImageVersionStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ImageVersionStatus = ImageVersionStatus'
  { fromImageVersionStatus ::
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

pattern ImageVersionStatus_CREATED :: ImageVersionStatus
pattern ImageVersionStatus_CREATED = ImageVersionStatus' "CREATED"

pattern ImageVersionStatus_CREATE_FAILED :: ImageVersionStatus
pattern ImageVersionStatus_CREATE_FAILED = ImageVersionStatus' "CREATE_FAILED"

pattern ImageVersionStatus_CREATING :: ImageVersionStatus
pattern ImageVersionStatus_CREATING = ImageVersionStatus' "CREATING"

pattern ImageVersionStatus_DELETE_FAILED :: ImageVersionStatus
pattern ImageVersionStatus_DELETE_FAILED = ImageVersionStatus' "DELETE_FAILED"

pattern ImageVersionStatus_DELETING :: ImageVersionStatus
pattern ImageVersionStatus_DELETING = ImageVersionStatus' "DELETING"

{-# COMPLETE
  ImageVersionStatus_CREATED,
  ImageVersionStatus_CREATE_FAILED,
  ImageVersionStatus_CREATING,
  ImageVersionStatus_DELETE_FAILED,
  ImageVersionStatus_DELETING,
  ImageVersionStatus'
  #-}
