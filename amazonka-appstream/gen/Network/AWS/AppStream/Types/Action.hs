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

import qualified Network.AWS.Prelude as Prelude

newtype Action = Action' {fromAction :: Prelude.Text}
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
