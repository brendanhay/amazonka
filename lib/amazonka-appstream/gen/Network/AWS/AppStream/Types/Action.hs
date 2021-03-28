{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Action
  ( Action
    ( Action'
    , ActionClipboardCopyFromLocalDevice
    , ActionClipboardCopyToLocalDevice
    , ActionFileUpload
    , ActionFileDownload
    , ActionPrintingToLocalDevice
    , fromAction
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Action = Action'{fromAction :: Core.Text}
                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                   Core.Generic)
                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                     Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                     Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern ActionClipboardCopyFromLocalDevice :: Action
pattern ActionClipboardCopyFromLocalDevice = Action' "CLIPBOARD_COPY_FROM_LOCAL_DEVICE"

pattern ActionClipboardCopyToLocalDevice :: Action
pattern ActionClipboardCopyToLocalDevice = Action' "CLIPBOARD_COPY_TO_LOCAL_DEVICE"

pattern ActionFileUpload :: Action
pattern ActionFileUpload = Action' "FILE_UPLOAD"

pattern ActionFileDownload :: Action
pattern ActionFileDownload = Action' "FILE_DOWNLOAD"

pattern ActionPrintingToLocalDevice :: Action
pattern ActionPrintingToLocalDevice = Action' "PRINTING_TO_LOCAL_DEVICE"

{-# COMPLETE 
  ActionClipboardCopyFromLocalDevice,

  ActionClipboardCopyToLocalDevice,

  ActionFileUpload,

  ActionFileDownload,

  ActionPrintingToLocalDevice,
  Action'
  #-}
