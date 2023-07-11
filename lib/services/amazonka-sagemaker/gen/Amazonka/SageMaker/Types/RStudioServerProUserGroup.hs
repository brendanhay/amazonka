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
-- Module      : Amazonka.SageMaker.Types.RStudioServerProUserGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RStudioServerProUserGroup
  ( RStudioServerProUserGroup
      ( ..,
        RStudioServerProUserGroup_R_STUDIO_ADMIN,
        RStudioServerProUserGroup_R_STUDIO_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RStudioServerProUserGroup = RStudioServerProUserGroup'
  { fromRStudioServerProUserGroup ::
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

pattern RStudioServerProUserGroup_R_STUDIO_ADMIN :: RStudioServerProUserGroup
pattern RStudioServerProUserGroup_R_STUDIO_ADMIN = RStudioServerProUserGroup' "R_STUDIO_ADMIN"

pattern RStudioServerProUserGroup_R_STUDIO_USER :: RStudioServerProUserGroup
pattern RStudioServerProUserGroup_R_STUDIO_USER = RStudioServerProUserGroup' "R_STUDIO_USER"

{-# COMPLETE
  RStudioServerProUserGroup_R_STUDIO_ADMIN,
  RStudioServerProUserGroup_R_STUDIO_USER,
  RStudioServerProUserGroup'
  #-}
