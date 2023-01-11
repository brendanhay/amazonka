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
-- Module      : Amazonka.FSx.Types.AdministrativeActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.AdministrativeActionType
  ( AdministrativeActionType
      ( ..,
        AdministrativeActionType_FILE_SYSTEM_ALIAS_ASSOCIATION,
        AdministrativeActionType_FILE_SYSTEM_ALIAS_DISASSOCIATION,
        AdministrativeActionType_FILE_SYSTEM_UPDATE,
        AdministrativeActionType_RELEASE_NFS_V3_LOCKS,
        AdministrativeActionType_SNAPSHOT_UPDATE,
        AdministrativeActionType_STORAGE_OPTIMIZATION,
        AdministrativeActionType_VOLUME_RESTORE,
        AdministrativeActionType_VOLUME_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the type of administrative action, as follows:
--
-- -   @FILE_SYSTEM_UPDATE@ - A file system update administrative action
--     initiated from the Amazon FSx console, API (@UpdateFileSystem@), or
--     CLI (@update-file-system@).
--
-- -   @STORAGE_OPTIMIZATION@ - After the @FILE_SYSTEM_UPDATE@ task to
--     increase a file system\'s storage capacity has been completed
--     successfully, a @STORAGE_OPTIMIZATION@ task starts.
--
--     -   For Windows and ONTAP, storage optimization is the process of
--         migrating the file system data to newer larger disks.
--
--     -   For Lustre, storage optimization consists of rebalancing the
--         data across the existing and newly added file servers.
--
--     You can track the storage-optimization progress using the
--     @ProgressPercent@ property. When @STORAGE_OPTIMIZATION@ has been
--     completed successfully, the parent @FILE_SYSTEM_UPDATE@ action
--     status changes to @COMPLETED@. For more information, see
--     <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-storage-capacity.html Managing storage capacity>
--     in the /Amazon FSx for Windows File Server User Guide/,
--     <https://docs.aws.amazon.com/fsx/latest/LustreGuide/managing-storage-capacity.html Managing storage and throughput capacity>
--     in the /Amazon FSx for Lustre User Guide/, and
--     <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-storage-capacity.html Managing storage capacity and provisioned IOPS>
--     in the /Amazon FSx for NetApp ONTAP User Guide/.
--
-- -   @FILE_SYSTEM_ALIAS_ASSOCIATION@ - A file system update to associate
--     a new Domain Name System (DNS) alias with the file system. For more
--     information, see
--     <https://docs.aws.amazon.com/fsx/latest/APIReference/API_AssociateFileSystemAliases.html AssociateFileSystemAliases>.
--
-- -   @FILE_SYSTEM_ALIAS_DISASSOCIATION@ - A file system update to
--     disassociate a DNS alias from the file system. For more information,
--     see
--     <https://docs.aws.amazon.com/fsx/latest/APIReference/API_DisassociateFileSystemAliases.html DisassociateFileSystemAliases>.
--
-- -   @VOLUME_UPDATE@ - A volume update to an Amazon FSx for NetApp ONTAP
--     or Amazon FSx for OpenZFS volume initiated from the Amazon FSx
--     console, API (@UpdateVolume@), or CLI (@update-volume@).
--
-- -   @VOLUME_RESTORE@ - An Amazon FSx for OpenZFS volume is returned to
--     the state saved by the specified snapshot, initiated from an API
--     (@RestoreVolumeFromSnapshot@) or CLI
--     (@restore-volume-from-snapshot@).
--
-- -   @SNAPSHOT_UPDATE@ - A snapshot update to an Amazon FSx for OpenZFS
--     volume initiated from the Amazon FSx console, API
--     (@UpdateSnapshot@), or CLI (@update-snapshot@).
--
-- -   @RELEASE_NFS_V3_LOCKS@ - Tracks the release of Network File System
--     (NFS) V3 locks on an Amazon FSx for OpenZFS file system.
newtype AdministrativeActionType = AdministrativeActionType'
  { fromAdministrativeActionType ::
      Data.Text
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

pattern AdministrativeActionType_FILE_SYSTEM_ALIAS_ASSOCIATION :: AdministrativeActionType
pattern AdministrativeActionType_FILE_SYSTEM_ALIAS_ASSOCIATION = AdministrativeActionType' "FILE_SYSTEM_ALIAS_ASSOCIATION"

pattern AdministrativeActionType_FILE_SYSTEM_ALIAS_DISASSOCIATION :: AdministrativeActionType
pattern AdministrativeActionType_FILE_SYSTEM_ALIAS_DISASSOCIATION = AdministrativeActionType' "FILE_SYSTEM_ALIAS_DISASSOCIATION"

pattern AdministrativeActionType_FILE_SYSTEM_UPDATE :: AdministrativeActionType
pattern AdministrativeActionType_FILE_SYSTEM_UPDATE = AdministrativeActionType' "FILE_SYSTEM_UPDATE"

pattern AdministrativeActionType_RELEASE_NFS_V3_LOCKS :: AdministrativeActionType
pattern AdministrativeActionType_RELEASE_NFS_V3_LOCKS = AdministrativeActionType' "RELEASE_NFS_V3_LOCKS"

pattern AdministrativeActionType_SNAPSHOT_UPDATE :: AdministrativeActionType
pattern AdministrativeActionType_SNAPSHOT_UPDATE = AdministrativeActionType' "SNAPSHOT_UPDATE"

pattern AdministrativeActionType_STORAGE_OPTIMIZATION :: AdministrativeActionType
pattern AdministrativeActionType_STORAGE_OPTIMIZATION = AdministrativeActionType' "STORAGE_OPTIMIZATION"

pattern AdministrativeActionType_VOLUME_RESTORE :: AdministrativeActionType
pattern AdministrativeActionType_VOLUME_RESTORE = AdministrativeActionType' "VOLUME_RESTORE"

pattern AdministrativeActionType_VOLUME_UPDATE :: AdministrativeActionType
pattern AdministrativeActionType_VOLUME_UPDATE = AdministrativeActionType' "VOLUME_UPDATE"

{-# COMPLETE
  AdministrativeActionType_FILE_SYSTEM_ALIAS_ASSOCIATION,
  AdministrativeActionType_FILE_SYSTEM_ALIAS_DISASSOCIATION,
  AdministrativeActionType_FILE_SYSTEM_UPDATE,
  AdministrativeActionType_RELEASE_NFS_V3_LOCKS,
  AdministrativeActionType_SNAPSHOT_UPDATE,
  AdministrativeActionType_STORAGE_OPTIMIZATION,
  AdministrativeActionType_VOLUME_RESTORE,
  AdministrativeActionType_VOLUME_UPDATE,
  AdministrativeActionType'
  #-}
