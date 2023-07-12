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
-- Module      : Amazonka.FSx.Types.OntapDeploymentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OntapDeploymentType
  ( OntapDeploymentType
      ( ..,
        OntapDeploymentType_MULTI_AZ_1,
        OntapDeploymentType_SINGLE_AZ_1
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OntapDeploymentType = OntapDeploymentType'
  { fromOntapDeploymentType ::
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

pattern OntapDeploymentType_MULTI_AZ_1 :: OntapDeploymentType
pattern OntapDeploymentType_MULTI_AZ_1 = OntapDeploymentType' "MULTI_AZ_1"

pattern OntapDeploymentType_SINGLE_AZ_1 :: OntapDeploymentType
pattern OntapDeploymentType_SINGLE_AZ_1 = OntapDeploymentType' "SINGLE_AZ_1"

{-# COMPLETE
  OntapDeploymentType_MULTI_AZ_1,
  OntapDeploymentType_SINGLE_AZ_1,
  OntapDeploymentType'
  #-}
