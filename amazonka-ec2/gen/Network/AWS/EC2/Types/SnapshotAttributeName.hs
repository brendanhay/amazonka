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
-- Module      : Network.AWS.EC2.Types.SnapshotAttributeName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotAttributeName
  ( SnapshotAttributeName
      ( ..,
        SnapshotAttributeName_CreateVolumePermission,
        SnapshotAttributeName_ProductCodes
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype SnapshotAttributeName = SnapshotAttributeName'
  { fromSnapshotAttributeName ::
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

pattern SnapshotAttributeName_CreateVolumePermission :: SnapshotAttributeName
pattern SnapshotAttributeName_CreateVolumePermission = SnapshotAttributeName' "createVolumePermission"

pattern SnapshotAttributeName_ProductCodes :: SnapshotAttributeName
pattern SnapshotAttributeName_ProductCodes = SnapshotAttributeName' "productCodes"

{-# COMPLETE
  SnapshotAttributeName_CreateVolumePermission,
  SnapshotAttributeName_ProductCodes,
  SnapshotAttributeName'
  #-}
