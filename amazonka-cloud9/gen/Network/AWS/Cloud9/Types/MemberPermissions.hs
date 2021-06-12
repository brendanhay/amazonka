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
-- Module      : Network.AWS.Cloud9.Types.MemberPermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.MemberPermissions
  ( MemberPermissions
      ( ..,
        MemberPermissions_Read_only,
        MemberPermissions_Read_write
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MemberPermissions = MemberPermissions'
  { fromMemberPermissions ::
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

pattern MemberPermissions_Read_only :: MemberPermissions
pattern MemberPermissions_Read_only = MemberPermissions' "read-only"

pattern MemberPermissions_Read_write :: MemberPermissions
pattern MemberPermissions_Read_write = MemberPermissions' "read-write"

{-# COMPLETE
  MemberPermissions_Read_only,
  MemberPermissions_Read_write,
  MemberPermissions'
  #-}
