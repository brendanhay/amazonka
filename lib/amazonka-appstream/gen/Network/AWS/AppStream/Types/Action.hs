-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Action
  ( Action
      ( Action',
        ClipboardCopyFromLocalDevice,
        ClipboardCopyToLocalDevice,
        FileDownload,
        FileUpload,
        PrintingToLocalDevice
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Action = Action' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ClipboardCopyFromLocalDevice :: Action
pattern ClipboardCopyFromLocalDevice = Action' "CLIPBOARD_COPY_FROM_LOCAL_DEVICE"

pattern ClipboardCopyToLocalDevice :: Action
pattern ClipboardCopyToLocalDevice = Action' "CLIPBOARD_COPY_TO_LOCAL_DEVICE"

pattern FileDownload :: Action
pattern FileDownload = Action' "FILE_DOWNLOAD"

pattern FileUpload :: Action
pattern FileUpload = Action' "FILE_UPLOAD"

pattern PrintingToLocalDevice :: Action
pattern PrintingToLocalDevice = Action' "PRINTING_TO_LOCAL_DEVICE"

{-# COMPLETE
  ClipboardCopyFromLocalDevice,
  ClipboardCopyToLocalDevice,
  FileDownload,
  FileUpload,
  PrintingToLocalDevice,
  Action'
  #-}
