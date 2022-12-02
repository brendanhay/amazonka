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
-- Module      : Amazonka.DeviceFarm.Types.TestGridSessionArtifactType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TestGridSessionArtifactType
  ( TestGridSessionArtifactType
      ( ..,
        TestGridSessionArtifactType_SELENIUM_LOG,
        TestGridSessionArtifactType_UNKNOWN,
        TestGridSessionArtifactType_VIDEO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TestGridSessionArtifactType = TestGridSessionArtifactType'
  { fromTestGridSessionArtifactType ::
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

pattern TestGridSessionArtifactType_SELENIUM_LOG :: TestGridSessionArtifactType
pattern TestGridSessionArtifactType_SELENIUM_LOG = TestGridSessionArtifactType' "SELENIUM_LOG"

pattern TestGridSessionArtifactType_UNKNOWN :: TestGridSessionArtifactType
pattern TestGridSessionArtifactType_UNKNOWN = TestGridSessionArtifactType' "UNKNOWN"

pattern TestGridSessionArtifactType_VIDEO :: TestGridSessionArtifactType
pattern TestGridSessionArtifactType_VIDEO = TestGridSessionArtifactType' "VIDEO"

{-# COMPLETE
  TestGridSessionArtifactType_SELENIUM_LOG,
  TestGridSessionArtifactType_UNKNOWN,
  TestGridSessionArtifactType_VIDEO,
  TestGridSessionArtifactType'
  #-}
