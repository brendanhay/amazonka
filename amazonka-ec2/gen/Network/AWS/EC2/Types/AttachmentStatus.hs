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
-- Module      : Network.AWS.EC2.Types.AttachmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttachmentStatus
  ( AttachmentStatus
      ( ..,
        AttachmentStatus_Attached,
        AttachmentStatus_Attaching,
        AttachmentStatus_Available,
        AttachmentStatus_Busy,
        AttachmentStatus_Detached,
        AttachmentStatus_Detaching
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype AttachmentStatus = AttachmentStatus'
  { fromAttachmentStatus ::
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

pattern AttachmentStatus_Attached :: AttachmentStatus
pattern AttachmentStatus_Attached = AttachmentStatus' "attached"

pattern AttachmentStatus_Attaching :: AttachmentStatus
pattern AttachmentStatus_Attaching = AttachmentStatus' "attaching"

pattern AttachmentStatus_Available :: AttachmentStatus
pattern AttachmentStatus_Available = AttachmentStatus' "available"

pattern AttachmentStatus_Busy :: AttachmentStatus
pattern AttachmentStatus_Busy = AttachmentStatus' "busy"

pattern AttachmentStatus_Detached :: AttachmentStatus
pattern AttachmentStatus_Detached = AttachmentStatus' "detached"

pattern AttachmentStatus_Detaching :: AttachmentStatus
pattern AttachmentStatus_Detaching = AttachmentStatus' "detaching"

{-# COMPLETE
  AttachmentStatus_Attached,
  AttachmentStatus_Attaching,
  AttachmentStatus_Available,
  AttachmentStatus_Busy,
  AttachmentStatus_Detached,
  AttachmentStatus_Detaching,
  AttachmentStatus'
  #-}
