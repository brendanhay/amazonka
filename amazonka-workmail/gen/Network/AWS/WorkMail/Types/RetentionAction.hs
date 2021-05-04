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
-- Module      : Network.AWS.WorkMail.Types.RetentionAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.RetentionAction
  ( RetentionAction
      ( ..,
        RetentionAction_DELETE,
        RetentionAction_NONE,
        RetentionAction_PERMANENTLY_DELETE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RetentionAction = RetentionAction'
  { fromRetentionAction ::
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

pattern RetentionAction_DELETE :: RetentionAction
pattern RetentionAction_DELETE = RetentionAction' "DELETE"

pattern RetentionAction_NONE :: RetentionAction
pattern RetentionAction_NONE = RetentionAction' "NONE"

pattern RetentionAction_PERMANENTLY_DELETE :: RetentionAction
pattern RetentionAction_PERMANENTLY_DELETE = RetentionAction' "PERMANENTLY_DELETE"

{-# COMPLETE
  RetentionAction_DELETE,
  RetentionAction_NONE,
  RetentionAction_PERMANENTLY_DELETE,
  RetentionAction'
  #-}
