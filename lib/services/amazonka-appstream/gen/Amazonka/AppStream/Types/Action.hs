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
-- Module      : Amazonka.AppStream.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.Action
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Action = Action' {fromAction :: Data.Text}
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
