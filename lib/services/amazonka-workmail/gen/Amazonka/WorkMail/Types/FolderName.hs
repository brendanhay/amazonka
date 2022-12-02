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
-- Module      : Amazonka.WorkMail.Types.FolderName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.FolderName
  ( FolderName
      ( ..,
        FolderName_DELETED_ITEMS,
        FolderName_DRAFTS,
        FolderName_INBOX,
        FolderName_JUNK_EMAIL,
        FolderName_SENT_ITEMS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FolderName = FolderName'
  { fromFolderName ::
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

pattern FolderName_DELETED_ITEMS :: FolderName
pattern FolderName_DELETED_ITEMS = FolderName' "DELETED_ITEMS"

pattern FolderName_DRAFTS :: FolderName
pattern FolderName_DRAFTS = FolderName' "DRAFTS"

pattern FolderName_INBOX :: FolderName
pattern FolderName_INBOX = FolderName' "INBOX"

pattern FolderName_JUNK_EMAIL :: FolderName
pattern FolderName_JUNK_EMAIL = FolderName' "JUNK_EMAIL"

pattern FolderName_SENT_ITEMS :: FolderName
pattern FolderName_SENT_ITEMS = FolderName' "SENT_ITEMS"

{-# COMPLETE
  FolderName_DELETED_ITEMS,
  FolderName_DRAFTS,
  FolderName_INBOX,
  FolderName_JUNK_EMAIL,
  FolderName_SENT_ITEMS,
  FolderName'
  #-}
