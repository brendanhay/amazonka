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
-- Module      : Amazonka.EMR.Types.InstanceCollectionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceCollectionType
  ( InstanceCollectionType
      ( ..,
        InstanceCollectionType_INSTANCE_FLEET,
        InstanceCollectionType_INSTANCE_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceCollectionType = InstanceCollectionType'
  { fromInstanceCollectionType ::
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

pattern InstanceCollectionType_INSTANCE_FLEET :: InstanceCollectionType
pattern InstanceCollectionType_INSTANCE_FLEET = InstanceCollectionType' "INSTANCE_FLEET"

pattern InstanceCollectionType_INSTANCE_GROUP :: InstanceCollectionType
pattern InstanceCollectionType_INSTANCE_GROUP = InstanceCollectionType' "INSTANCE_GROUP"

{-# COMPLETE
  InstanceCollectionType_INSTANCE_FLEET,
  InstanceCollectionType_INSTANCE_GROUP,
  InstanceCollectionType'
  #-}
