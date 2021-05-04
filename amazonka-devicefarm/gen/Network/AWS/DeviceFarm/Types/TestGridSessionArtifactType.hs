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
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
  ( TestGridSessionArtifactType
      ( ..,
        TestGridSessionArtifactType_SELENIUM_LOG,
        TestGridSessionArtifactType_UNKNOWN,
        TestGridSessionArtifactType_VIDEO
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TestGridSessionArtifactType = TestGridSessionArtifactType'
  { fromTestGridSessionArtifactType ::
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
