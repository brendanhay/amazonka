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
-- Module      : Amazonka.FSx.Types.OpenZFSDeploymentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSDeploymentType
  ( OpenZFSDeploymentType
      ( ..,
        OpenZFSDeploymentType_SINGLE_AZ_1,
        OpenZFSDeploymentType_SINGLE_AZ_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OpenZFSDeploymentType = OpenZFSDeploymentType'
  { fromOpenZFSDeploymentType ::
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

pattern OpenZFSDeploymentType_SINGLE_AZ_1 :: OpenZFSDeploymentType
pattern OpenZFSDeploymentType_SINGLE_AZ_1 = OpenZFSDeploymentType' "SINGLE_AZ_1"

pattern OpenZFSDeploymentType_SINGLE_AZ_2 :: OpenZFSDeploymentType
pattern OpenZFSDeploymentType_SINGLE_AZ_2 = OpenZFSDeploymentType' "SINGLE_AZ_2"

{-# COMPLETE
  OpenZFSDeploymentType_SINGLE_AZ_1,
  OpenZFSDeploymentType_SINGLE_AZ_2,
  OpenZFSDeploymentType'
  #-}
