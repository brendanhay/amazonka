{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessKeyMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessKeyMetadata where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.StatusType
import qualified Network.AWS.Lens as Lens

-- | Contains information about an AWS access key, without its secret key.
--
-- This data type is used as a response element in the ListAccessKeys
-- operation.
--
-- /See:/ 'newAccessKeyMetadata' smart constructor.
data AccessKeyMetadata = AccessKeyMetadata'
  { -- | The status of the access key. @Active@ means that the key is valid for
    -- API calls; @Inactive@ means it is not.
    status :: Core.Maybe StatusType,
    -- | The date when the access key was created.
    createDate :: Core.Maybe Core.ISO8601,
    -- | The ID for this access key.
    accessKeyId :: Core.Maybe Core.AccessKey,
    -- | The name of the IAM user that the key is associated with.
    userName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessKeyMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'accessKeyMetadata_status' - The status of the access key. @Active@ means that the key is valid for
-- API calls; @Inactive@ means it is not.
--
-- 'createDate', 'accessKeyMetadata_createDate' - The date when the access key was created.
--
-- 'accessKeyId', 'accessKeyMetadata_accessKeyId' - The ID for this access key.
--
-- 'userName', 'accessKeyMetadata_userName' - The name of the IAM user that the key is associated with.
newAccessKeyMetadata ::
  AccessKeyMetadata
newAccessKeyMetadata =
  AccessKeyMetadata'
    { status = Core.Nothing,
      createDate = Core.Nothing,
      accessKeyId = Core.Nothing,
      userName = Core.Nothing
    }

-- | The status of the access key. @Active@ means that the key is valid for
-- API calls; @Inactive@ means it is not.
accessKeyMetadata_status :: Lens.Lens' AccessKeyMetadata (Core.Maybe StatusType)
accessKeyMetadata_status = Lens.lens (\AccessKeyMetadata' {status} -> status) (\s@AccessKeyMetadata' {} a -> s {status = a} :: AccessKeyMetadata)

-- | The date when the access key was created.
accessKeyMetadata_createDate :: Lens.Lens' AccessKeyMetadata (Core.Maybe Core.UTCTime)
accessKeyMetadata_createDate = Lens.lens (\AccessKeyMetadata' {createDate} -> createDate) (\s@AccessKeyMetadata' {} a -> s {createDate = a} :: AccessKeyMetadata) Core.. Lens.mapping Core._Time

-- | The ID for this access key.
accessKeyMetadata_accessKeyId :: Lens.Lens' AccessKeyMetadata (Core.Maybe Core.AccessKey)
accessKeyMetadata_accessKeyId = Lens.lens (\AccessKeyMetadata' {accessKeyId} -> accessKeyId) (\s@AccessKeyMetadata' {} a -> s {accessKeyId = a} :: AccessKeyMetadata)

-- | The name of the IAM user that the key is associated with.
accessKeyMetadata_userName :: Lens.Lens' AccessKeyMetadata (Core.Maybe Core.Text)
accessKeyMetadata_userName = Lens.lens (\AccessKeyMetadata' {userName} -> userName) (\s@AccessKeyMetadata' {} a -> s {userName = a} :: AccessKeyMetadata)

instance Core.FromXML AccessKeyMetadata where
  parseXML x =
    AccessKeyMetadata'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "AccessKeyId")
      Core.<*> (x Core..@? "UserName")

instance Core.Hashable AccessKeyMetadata

instance Core.NFData AccessKeyMetadata
