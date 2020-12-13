{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupTypeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupTypeFilter
  ( BackupTypeFilter
      ( BackupTypeFilter',
        BTFUser,
        BTFSystem,
        BTFAWSBackup,
        BTFAll
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BackupTypeFilter = BackupTypeFilter' Lude.Text
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

pattern BTFUser :: BackupTypeFilter
pattern BTFUser = BackupTypeFilter' "USER"

pattern BTFSystem :: BackupTypeFilter
pattern BTFSystem = BackupTypeFilter' "SYSTEM"

pattern BTFAWSBackup :: BackupTypeFilter
pattern BTFAWSBackup = BackupTypeFilter' "AWS_BACKUP"

pattern BTFAll :: BackupTypeFilter
pattern BTFAll = BackupTypeFilter' "ALL"

{-# COMPLETE
  BTFUser,
  BTFSystem,
  BTFAWSBackup,
  BTFAll,
  BackupTypeFilter'
  #-}
