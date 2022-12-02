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
-- Module      : Amazonka.RAM.Types.ResourceShareFeatureSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShareFeatureSet
  ( ResourceShareFeatureSet
      ( ..,
        ResourceShareFeatureSet_CREATED_FROM_POLICY,
        ResourceShareFeatureSet_PROMOTING_TO_STANDARD,
        ResourceShareFeatureSet_STANDARD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceShareFeatureSet = ResourceShareFeatureSet'
  { fromResourceShareFeatureSet ::
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

pattern ResourceShareFeatureSet_CREATED_FROM_POLICY :: ResourceShareFeatureSet
pattern ResourceShareFeatureSet_CREATED_FROM_POLICY = ResourceShareFeatureSet' "CREATED_FROM_POLICY"

pattern ResourceShareFeatureSet_PROMOTING_TO_STANDARD :: ResourceShareFeatureSet
pattern ResourceShareFeatureSet_PROMOTING_TO_STANDARD = ResourceShareFeatureSet' "PROMOTING_TO_STANDARD"

pattern ResourceShareFeatureSet_STANDARD :: ResourceShareFeatureSet
pattern ResourceShareFeatureSet_STANDARD = ResourceShareFeatureSet' "STANDARD"

{-# COMPLETE
  ResourceShareFeatureSet_CREATED_FROM_POLICY,
  ResourceShareFeatureSet_PROMOTING_TO_STANDARD,
  ResourceShareFeatureSet_STANDARD,
  ResourceShareFeatureSet'
  #-}
