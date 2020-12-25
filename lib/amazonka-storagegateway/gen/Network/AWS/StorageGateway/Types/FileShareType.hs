{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.FileShareType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.FileShareType
  ( FileShareType
      ( FileShareType',
        FileShareTypeNfs,
        FileShareTypeSmb,
        fromFileShareType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The type of the file share.
newtype FileShareType = FileShareType'
  { fromFileShareType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern FileShareTypeNfs :: FileShareType
pattern FileShareTypeNfs = FileShareType' "NFS"

pattern FileShareTypeSmb :: FileShareType
pattern FileShareTypeSmb = FileShareType' "SMB"

{-# COMPLETE
  FileShareTypeNfs,
  FileShareTypeSmb,
  FileShareType'
  #-}
