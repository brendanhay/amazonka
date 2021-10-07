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
-- Module      : Network.AWS.Firehose.Types.RedshiftS3BackupMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftS3BackupMode
  ( RedshiftS3BackupMode
      ( ..,
        RedshiftS3BackupMode_Disabled,
        RedshiftS3BackupMode_Enabled
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RedshiftS3BackupMode = RedshiftS3BackupMode'
  { fromRedshiftS3BackupMode ::
      Core.Text
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

pattern RedshiftS3BackupMode_Disabled :: RedshiftS3BackupMode
pattern RedshiftS3BackupMode_Disabled = RedshiftS3BackupMode' "Disabled"

pattern RedshiftS3BackupMode_Enabled :: RedshiftS3BackupMode
pattern RedshiftS3BackupMode_Enabled = RedshiftS3BackupMode' "Enabled"

{-# COMPLETE
  RedshiftS3BackupMode_Disabled,
  RedshiftS3BackupMode_Enabled,
  RedshiftS3BackupMode'
  #-}
