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
-- Module      : Network.AWS.AppStream.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Action
  ( Action
      ( ..,
        Action_CLIPBOARD_COPY_FROM_LOCAL_DEVICE,
        Action_CLIPBOARD_COPY_TO_LOCAL_DEVICE,
        Action_DOMAIN_PASSWORD_SIGNIN,
        Action_DOMAIN_SMART_CARD_SIGNIN,
        Action_FILE_DOWNLOAD,
        Action_FILE_UPLOAD,
        Action_PRINTING_TO_LOCAL_DEVICE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Action = Action' {fromAction :: Core.Text}
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

pattern Action_CLIPBOARD_COPY_FROM_LOCAL_DEVICE :: Action
pattern Action_CLIPBOARD_COPY_FROM_LOCAL_DEVICE = Action' "CLIPBOARD_COPY_FROM_LOCAL_DEVICE"

pattern Action_CLIPBOARD_COPY_TO_LOCAL_DEVICE :: Action
pattern Action_CLIPBOARD_COPY_TO_LOCAL_DEVICE = Action' "CLIPBOARD_COPY_TO_LOCAL_DEVICE"

pattern Action_DOMAIN_PASSWORD_SIGNIN :: Action
pattern Action_DOMAIN_PASSWORD_SIGNIN = Action' "DOMAIN_PASSWORD_SIGNIN"

pattern Action_DOMAIN_SMART_CARD_SIGNIN :: Action
pattern Action_DOMAIN_SMART_CARD_SIGNIN = Action' "DOMAIN_SMART_CARD_SIGNIN"

pattern Action_FILE_DOWNLOAD :: Action
pattern Action_FILE_DOWNLOAD = Action' "FILE_DOWNLOAD"

pattern Action_FILE_UPLOAD :: Action
pattern Action_FILE_UPLOAD = Action' "FILE_UPLOAD"

pattern Action_PRINTING_TO_LOCAL_DEVICE :: Action
pattern Action_PRINTING_TO_LOCAL_DEVICE = Action' "PRINTING_TO_LOCAL_DEVICE"

{-# COMPLETE
  Action_CLIPBOARD_COPY_FROM_LOCAL_DEVICE,
  Action_CLIPBOARD_COPY_TO_LOCAL_DEVICE,
  Action_DOMAIN_PASSWORD_SIGNIN,
  Action_DOMAIN_SMART_CARD_SIGNIN,
  Action_FILE_DOWNLOAD,
  Action_FILE_UPLOAD,
  Action_PRINTING_TO_LOCAL_DEVICE,
  Action'
  #-}
