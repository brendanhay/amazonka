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
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetType
  ( ChangeSetType
      ( ..,
        ChangeSetType_CREATE,
        ChangeSetType_IMPORT,
        ChangeSetType_UPDATE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ChangeSetType = ChangeSetType'
  { fromChangeSetType ::
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

pattern ChangeSetType_CREATE :: ChangeSetType
pattern ChangeSetType_CREATE = ChangeSetType' "CREATE"

pattern ChangeSetType_IMPORT :: ChangeSetType
pattern ChangeSetType_IMPORT = ChangeSetType' "IMPORT"

pattern ChangeSetType_UPDATE :: ChangeSetType
pattern ChangeSetType_UPDATE = ChangeSetType' "UPDATE"

{-# COMPLETE
  ChangeSetType_CREATE,
  ChangeSetType_IMPORT,
  ChangeSetType_UPDATE,
  ChangeSetType'
  #-}
