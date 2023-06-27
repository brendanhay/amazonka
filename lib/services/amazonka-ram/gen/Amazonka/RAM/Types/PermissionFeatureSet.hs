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
-- Module      : Amazonka.RAM.Types.PermissionFeatureSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.PermissionFeatureSet
  ( PermissionFeatureSet
      ( ..,
        PermissionFeatureSet_CREATED_FROM_POLICY,
        PermissionFeatureSet_PROMOTING_TO_STANDARD,
        PermissionFeatureSet_STANDARD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PermissionFeatureSet = PermissionFeatureSet'
  { fromPermissionFeatureSet ::
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

pattern PermissionFeatureSet_CREATED_FROM_POLICY :: PermissionFeatureSet
pattern PermissionFeatureSet_CREATED_FROM_POLICY = PermissionFeatureSet' "CREATED_FROM_POLICY"

pattern PermissionFeatureSet_PROMOTING_TO_STANDARD :: PermissionFeatureSet
pattern PermissionFeatureSet_PROMOTING_TO_STANDARD = PermissionFeatureSet' "PROMOTING_TO_STANDARD"

pattern PermissionFeatureSet_STANDARD :: PermissionFeatureSet
pattern PermissionFeatureSet_STANDARD = PermissionFeatureSet' "STANDARD"

{-# COMPLETE
  PermissionFeatureSet_CREATED_FROM_POLICY,
  PermissionFeatureSet_PROMOTING_TO_STANDARD,
  PermissionFeatureSet_STANDARD,
  PermissionFeatureSet'
  #-}
