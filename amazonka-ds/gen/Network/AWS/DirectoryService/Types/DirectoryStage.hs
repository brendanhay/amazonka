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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryStage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryStage
  ( DirectoryStage
      ( ..,
        DirectoryStage_Active,
        DirectoryStage_Created,
        DirectoryStage_Creating,
        DirectoryStage_Deleted,
        DirectoryStage_Deleting,
        DirectoryStage_Failed,
        DirectoryStage_Impaired,
        DirectoryStage_Inoperable,
        DirectoryStage_Requested,
        DirectoryStage_RestoreFailed,
        DirectoryStage_Restoring
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DirectoryStage = DirectoryStage'
  { fromDirectoryStage ::
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

pattern DirectoryStage_Active :: DirectoryStage
pattern DirectoryStage_Active = DirectoryStage' "Active"

pattern DirectoryStage_Created :: DirectoryStage
pattern DirectoryStage_Created = DirectoryStage' "Created"

pattern DirectoryStage_Creating :: DirectoryStage
pattern DirectoryStage_Creating = DirectoryStage' "Creating"

pattern DirectoryStage_Deleted :: DirectoryStage
pattern DirectoryStage_Deleted = DirectoryStage' "Deleted"

pattern DirectoryStage_Deleting :: DirectoryStage
pattern DirectoryStage_Deleting = DirectoryStage' "Deleting"

pattern DirectoryStage_Failed :: DirectoryStage
pattern DirectoryStage_Failed = DirectoryStage' "Failed"

pattern DirectoryStage_Impaired :: DirectoryStage
pattern DirectoryStage_Impaired = DirectoryStage' "Impaired"

pattern DirectoryStage_Inoperable :: DirectoryStage
pattern DirectoryStage_Inoperable = DirectoryStage' "Inoperable"

pattern DirectoryStage_Requested :: DirectoryStage
pattern DirectoryStage_Requested = DirectoryStage' "Requested"

pattern DirectoryStage_RestoreFailed :: DirectoryStage
pattern DirectoryStage_RestoreFailed = DirectoryStage' "RestoreFailed"

pattern DirectoryStage_Restoring :: DirectoryStage
pattern DirectoryStage_Restoring = DirectoryStage' "Restoring"

{-# COMPLETE
  DirectoryStage_Active,
  DirectoryStage_Created,
  DirectoryStage_Creating,
  DirectoryStage_Deleted,
  DirectoryStage_Deleting,
  DirectoryStage_Failed,
  DirectoryStage_Impaired,
  DirectoryStage_Inoperable,
  DirectoryStage_Requested,
  DirectoryStage_RestoreFailed,
  DirectoryStage_Restoring,
  DirectoryStage'
  #-}
