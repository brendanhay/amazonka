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
-- Module      : Network.AWS.Glue.Types.WorkerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkerType
  ( WorkerType
      ( ..,
        WorkerType_G_1X,
        WorkerType_G_2X,
        WorkerType_Standard
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype WorkerType = WorkerType'
  { fromWorkerType ::
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

pattern WorkerType_G_1X :: WorkerType
pattern WorkerType_G_1X = WorkerType' "G.1X"

pattern WorkerType_G_2X :: WorkerType
pattern WorkerType_G_2X = WorkerType' "G.2X"

pattern WorkerType_Standard :: WorkerType
pattern WorkerType_Standard = WorkerType' "Standard"

{-# COMPLETE
  WorkerType_G_1X,
  WorkerType_G_2X,
  WorkerType_Standard,
  WorkerType'
  #-}
