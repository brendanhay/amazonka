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
-- Module      : Network.AWS.Glue.Types.JobBookmarksEncryptionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarksEncryptionMode
  ( JobBookmarksEncryptionMode
      ( ..,
        JobBookmarksEncryptionMode_CSE_KMS,
        JobBookmarksEncryptionMode_DISABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype JobBookmarksEncryptionMode = JobBookmarksEncryptionMode'
  { fromJobBookmarksEncryptionMode ::
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

pattern JobBookmarksEncryptionMode_CSE_KMS :: JobBookmarksEncryptionMode
pattern JobBookmarksEncryptionMode_CSE_KMS = JobBookmarksEncryptionMode' "CSE-KMS"

pattern JobBookmarksEncryptionMode_DISABLED :: JobBookmarksEncryptionMode
pattern JobBookmarksEncryptionMode_DISABLED = JobBookmarksEncryptionMode' "DISABLED"

{-# COMPLETE
  JobBookmarksEncryptionMode_CSE_KMS,
  JobBookmarksEncryptionMode_DISABLED,
  JobBookmarksEncryptionMode'
  #-}
