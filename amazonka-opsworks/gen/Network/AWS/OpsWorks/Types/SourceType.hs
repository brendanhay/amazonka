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
-- Module      : Network.AWS.OpsWorks.Types.SourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_Archive,
        SourceType_Git,
        SourceType_S3,
        SourceType_Svn
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SourceType = SourceType'
  { fromSourceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern SourceType_Archive :: SourceType
pattern SourceType_Archive = SourceType' "archive"

pattern SourceType_Git :: SourceType
pattern SourceType_Git = SourceType' "git"

pattern SourceType_S3 :: SourceType
pattern SourceType_S3 = SourceType' "s3"

pattern SourceType_Svn :: SourceType
pattern SourceType_Svn = SourceType' "svn"

{-# COMPLETE
  SourceType_Archive,
  SourceType_Git,
  SourceType_S3,
  SourceType_Svn,
  SourceType'
  #-}
